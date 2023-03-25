use core::ops::ControlFlow;

use clippy_utils::diagnostics::span_lint_and_sugg;
use clippy_utils::source::snippet;
use clippy_utils::ty::is_type_diagnostic_item;
use clippy_utils::{peel_blocks, path_to_local_id, get_enclosing_block};
use rustc_middle::hir::nested_filter;
use rustc_errors::Applicability;
use rustc_hir::{self as hir, HirId};
use rustc_hir::intravisit::{Visitor, walk_expr};
use rustc_hir::{Expr};
use rustc_lint::LateContext;
use rustc_span::{Span, symbol::sym};

use super::MAP_THEN_UNWRAP;

/// Runs the given function for each path expression referencing the given local which occur after
/// the given expression.
pub fn find_each_local_use_in_expr<'tcx, B>(
    cx: &LateContext<'tcx>,
    local_id: HirId,
    expr_id: HirId,
    f: impl FnMut(&'tcx Expr<'tcx>) -> ControlFlow<B>,
) -> ControlFlow<B> {
    struct V<'cx, 'tcx, F, B> {
        cx: &'cx LateContext<'tcx>,
        local_id: HirId,
        expr_id: HirId,
        found: bool,
        res: ControlFlow<B>,
        f: F,
    }
    impl<'cx, 'tcx, F: FnMut(&'tcx Expr<'tcx>) -> ControlFlow<B>, B> Visitor<'tcx> for V<'cx, 'tcx, F, B> {
        type NestedFilter = nested_filter::OnlyBodies;
        fn nested_visit_map(&mut self) -> Self::Map {
            self.cx.tcx.hir()
        }

        fn visit_expr(&mut self, e: &'tcx Expr<'tcx>) {
            if !self.found {
                if e.hir_id == self.expr_id {
                    self.found = true;
                } else {
                    walk_expr(self, e);
                }
                return;
            }
            if self.res.is_break() {
                return;
            }
            if path_to_local_id(e, self.local_id) {
                self.res = (self.f)(e);
            } else {
                walk_expr(self, e);
            }
        }

    }

    if let Some(b) = get_enclosing_block(cx, local_id) {
        let mut v = V {
            cx,
            local_id,
            expr_id,
            found: false,
            res: ControlFlow::Continue(()),
            f,
        };
        v.visit_block(b);
        v.res
    } else {
        ControlFlow::Continue(())
    }
}

pub(super) fn check<'tcx>(
    cx: &LateContext<'tcx>,
    unwrap_expr: &Expr<'_>,
    recv: &'tcx Expr<'tcx>,
    map_arg: &'tcx Expr<'_>,
    map_span: Span,
) {
    let ty = cx.typeck_results().expr_ty(recv);
    // get type of x (we later check if it's Option or Result)
    // lint if the caller of `map()` is an `Option`
    let is_option = is_type_diagnostic_item(cx, ty, sym::Option);
    // lint if the caller of `map()` is a `Result`
    let is_result = is_type_diagnostic_item(cx, ty, sym::Result);

    if !is_result && !is_option {
        return;
    }

    let msg = "called `map(…).unwrap()` on an `Option` or `Result` value. This can be done more directly by calling \
               `.unwrap() …` instead";

    let self_snippet = snippet(cx, recv.span, "…");
    std::thread::sleep(std::time::Duration::new(20, 0));
    if_chain! {
        if let hir::ExprKind::Closure(&hir::Closure { body, .. }) = map_arg.kind;
        let body = cx.tcx.hir().body(body);
        if let [map_params] = body.params;
        let closure_expr = peel_blocks(body.value);
        then {
            // let pre_arg_span = body_span.with_hi(arg_char.span.lo());
            // let post_arg_span = body_span.with_lo(arg_char.span.hi());

            let sugg = format!("{}.unwrap()", self_snippet);
            return span_lint_and_sugg(
                cx,
                MAP_THEN_UNWRAP,
                unwrap_expr.span.with_lo(map_span.lo()),
                msg,
                "try using `unwrap` directly",
                sugg,
                Applicability::MachineApplicable,
            );
        }
    }

    let func_snippet = snippet(cx, map_arg.span, "…");
    span_lint_and_sugg(
        cx,
        MAP_THEN_UNWRAP,
        unwrap_expr.span.with_lo(map_span.lo()),
        msg,
        "try using `unwrap` directly",
        format!("{0}.unwrap() {1}", self_snippet, func_snippet),
        Applicability::MachineApplicable,
    );
}
