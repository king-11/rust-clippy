// autogenerated. Please look at /clippy_dev/src/update_lints.rs

macro_rules! include_lint {
    ($file_name: expr) => {
        include_str!($file_name)
    };
}

macro_rules! docs {
    ($($lint_name: expr,)*) => {
        pub fn explain(lint: &str) {
            println!("{}", match lint {
                $(
                    $lint_name => include_lint!(concat!("docs/", concat!($lint_name, ".txt"))),
                )*
                _ => "unknown lint",
            })
        }
    }
}

docs! {
    "absurd_extreme_comparisons",
    "alloc_instead_of_core",
    "allow_attributes_without_reason",
    "almost_complete_letter_range",
    "almost_swapped",
    "approx_constant",
    "arithmetic_side_effects",
    "as_conversions",
    "as_underscore",
    "assertions_on_constants",
    "assertions_on_result_states",
    "assign_op_pattern",
    "async_yields_async",
    "await_holding_invalid_type",
    "await_holding_lock",
    "await_holding_refcell_ref",
    "bad_bit_mask",
    "bind_instead_of_map",
    "blanket_clippy_restriction_lints",
    "blocks_in_if_conditions",
    "bool_assert_comparison",
    "bool_comparison",
    "bool_to_int_with_if",
    "borrow_as_ptr",
    "borrow_deref_ref",
    "borrow_interior_mutable_const",
    "borrowed_box",
    "box_collection",
    "boxed_local",
    "branches_sharing_code",
    "builtin_type_shadow",
    "bytes_count_to_len",
    "bytes_nth",
    "cargo_common_metadata",
    "case_sensitive_file_extension_comparisons",
    "cast_abs_to_unsigned",
    "cast_enum_constructor",
    "cast_enum_truncation",
    "cast_lossless",
    "cast_possible_truncation",
    "cast_possible_wrap",
    "cast_precision_loss",
    "cast_ptr_alignment",
    "cast_ref_to_mut",
    "cast_sign_loss",
    "cast_slice_different_sizes",
    "cast_slice_from_raw_parts",
    "char_lit_as_u8",
    "chars_last_cmp",
    "chars_next_cmp",
    "checked_conversions",
    "clone_double_ref",
    "clone_on_copy",
    "clone_on_ref_ptr",
    "cloned_instead_of_copied",
    "cmp_nan",
    "cmp_null",
    "cmp_owned",
    "cognitive_complexity",
    "collapsible_else_if",
    "collapsible_if",
    "collapsible_match",
    "collapsible_str_replace",
    "comparison_chain",
    "comparison_to_empty",
    "copy_iterator",
    "crate_in_macro_def",
    "create_dir",
    "crosspointer_transmute",
    "dbg_macro",
    "debug_assert_with_mut_call",
    "decimal_literal_representation",
    "declare_interior_mutable_const",
    "default_instead_of_iter_empty",
    "default_numeric_fallback",
    "default_trait_access",
    "default_union_representation",
    "deprecated_cfg_attr",
    "deprecated_semver",
    "deref_addrof",
    "deref_by_slicing",
    "derivable_impls",
    "derive_hash_xor_eq",
    "derive_ord_xor_partial_ord",
    "derive_partial_eq_without_eq",
    "disallowed_methods",
    "disallowed_names",
    "disallowed_script_idents",
    "disallowed_types",
    "diverging_sub_expression",
    "doc_link_with_quotes",
    "doc_markdown",
    "double_comparisons",
    "double_must_use",
    "double_neg",
    "double_parens",
    "drop_copy",
    "drop_non_drop",
    "drop_ref",
    "duplicate_mod",
    "duplicate_underscore_argument",
    "duration_subsec",
    "else_if_without_else",
    "empty_drop",
    "empty_enum",
    "empty_line_after_outer_attr",
    "empty_loop",
    "empty_structs_with_brackets",
    "enum_clike_unportable_variant",
    "enum_glob_use",
    "enum_variant_names",
    "eq_op",
    "equatable_if_let",
    "erasing_op",
    "err_expect",
    "excessive_precision",
    "exhaustive_enums",
    "exhaustive_structs",
    "exit",
    "expect_fun_call",
    "expect_used",
    "expl_impl_clone_on_copy",
    "explicit_auto_deref",
    "explicit_counter_loop",
    "explicit_deref_methods",
    "explicit_into_iter_loop",
    "explicit_iter_loop",
    "explicit_write",
    "extend_with_drain",
    "extra_unused_lifetimes",
    "fallible_impl_from",
    "field_reassign_with_default",
    "filetype_is_file",
    "filter_map_identity",
    "filter_map_next",
    "filter_next",
    "flat_map_identity",
    "flat_map_option",
    "float_arithmetic",
    "float_cmp",
    "float_cmp_const",
    "float_equality_without_abs",
    "fn_address_comparisons",
    "fn_params_excessive_bools",
    "fn_to_numeric_cast",
    "fn_to_numeric_cast_any",
    "fn_to_numeric_cast_with_truncation",
    "for_kv_map",
    "for_loops_over_fallibles",
    "forget_copy",
    "forget_non_drop",
    "forget_ref",
    "format_in_format_args",
    "format_push_string",
    "from_iter_instead_of_collect",
    "from_over_into",
    "from_str_radix_10",
    "future_not_send",
    "get_first",
    "get_last_with_len",
    "get_unwrap",
    "identity_op",
    "if_let_mutex",
    "if_not_else",
    "if_same_then_else",
    "if_then_some_else_none",
    "ifs_same_cond",
    "implicit_clone",
    "implicit_hasher",
    "implicit_return",
    "implicit_saturating_sub",
    "imprecise_flops",
    "inconsistent_digit_grouping",
    "inconsistent_struct_constructor",
    "index_refutable_slice",
    "indexing_slicing",
    "ineffective_bit_mask",
    "inefficient_to_string",
    "infallible_destructuring_match",
    "infinite_iter",
    "inherent_to_string",
    "inherent_to_string_shadow_display",
    "init_numbered_fields",
    "inline_always",
    "inline_asm_x86_att_syntax",
    "inline_asm_x86_intel_syntax",
    "inline_fn_without_body",
    "inspect_for_each",
    "int_plus_one",
    "integer_arithmetic",
    "integer_division",
    "into_iter_on_ref",
    "invalid_null_ptr_usage",
    "invalid_regex",
    "invalid_upcast_comparisons",
    "invalid_utf8_in_unchecked",
    "invisible_characters",
    "is_digit_ascii_radix",
    "items_after_statements",
    "iter_cloned_collect",
    "iter_count",
    "iter_next_loop",
    "iter_next_slice",
    "iter_not_returning_iterator",
    "iter_nth",
    "iter_nth_zero",
    "iter_on_empty_collections",
    "iter_on_single_items",
    "iter_overeager_cloned",
    "iter_skip_next",
    "iter_with_drain",
    "iterator_step_by_zero",
    "just_underscores_and_digits",
    "large_const_arrays",
    "large_digit_groups",
    "large_enum_variant",
    "large_include_file",
    "large_stack_arrays",
    "large_types_passed_by_value",
    "len_without_is_empty",
    "len_zero",
    "let_and_return",
    "let_underscore_drop",
    "let_underscore_lock",
    "let_underscore_must_use",
    "let_unit_value",
    "linkedlist",
    "lossy_float_literal",
    "macro_use_imports",
    "main_recursion",
    "manual_assert",
    "manual_async_fn",
    "manual_bits",
    "manual_filter_map",
    "manual_find",
    "manual_find_map",
    "manual_flatten",
    "manual_instant_elapsed",
    "manual_map",
    "manual_memcpy",
    "manual_non_exhaustive",
    "manual_ok_or",
    "manual_range_contains",
    "manual_rem_euclid",
    "manual_retain",
    "manual_saturating_arithmetic",
    "manual_split_once",
    "manual_str_repeat",
    "manual_string_new",
    "manual_strip",
    "manual_swap",
    "manual_unwrap_or",
    "many_single_char_names",
    "map_clone",
    "map_collect_result_unit",
    "map_entry",
    "map_err_ignore",
    "map_flatten",
    "map_identity",
    "map_then_unwrap",
    "map_unwrap_or",
    "match_as_ref",
    "match_bool",
    "match_like_matches_macro",
    "match_on_vec_items",
    "match_overlapping_arm",
    "match_ref_pats",
    "match_result_ok",
    "match_same_arms",
    "match_single_binding",
    "match_str_case_mismatch",
    "match_wild_err_arm",
    "match_wildcard_for_single_variants",
    "maybe_infinite_iter",
    "mem_forget",
    "mem_replace_option_with_none",
    "mem_replace_with_default",
    "mem_replace_with_uninit",
    "min_max",
    "mismatched_target_os",
    "mismatching_type_param_order",
    "misrefactored_assign_op",
    "missing_const_for_fn",
    "missing_docs_in_private_items",
    "missing_enforced_import_renames",
    "missing_errors_doc",
    "missing_inline_in_public_items",
    "missing_panics_doc",
    "missing_safety_doc",
    "missing_spin_loop",
    "mistyped_literal_suffixes",
    "mixed_case_hex_literals",
    "mixed_read_write_in_expression",
    "mod_module_files",
    "module_inception",
    "module_name_repetitions",
    "modulo_arithmetic",
    "modulo_one",
    "multi_assignments",
    "multiple_crate_versions",
    "multiple_inherent_impl",
    "must_use_candidate",
    "must_use_unit",
    "mut_from_ref",
    "mut_mut",
    "mut_mutex_lock",
    "mut_range_bound",
    "mutable_key_type",
    "mutex_atomic",
    "mutex_integer",
    "naive_bytecount",
    "needless_arbitrary_self_type",
    "needless_bitwise_bool",
    "needless_bool",
    "needless_borrow",
    "needless_borrowed_reference",
    "needless_collect",
    "needless_continue",
    "needless_doctest_main",
    "needless_for_each",
    "needless_late_init",
    "needless_lifetimes",
    "needless_match",
    "needless_option_as_deref",
    "needless_option_take",
    "needless_parens_on_range_literals",
    "needless_pass_by_value",
    "needless_question_mark",
    "needless_range_loop",
    "needless_return",
    "needless_splitn",
    "needless_update",
    "neg_cmp_op_on_partial_ord",
    "neg_multiply",
    "negative_feature_names",
    "never_loop",
    "new_ret_no_self",
    "new_without_default",
    "no_effect",
    "no_effect_replace",
    "no_effect_underscore_binding",
    "non_ascii_literal",
    "non_octal_unix_permissions",
    "non_send_fields_in_send_ty",
    "nonminimal_bool",
    "nonsensical_open_options",
    "nonstandard_macro_braces",
    "not_unsafe_ptr_arg_deref",
    "obfuscated_if_else",
    "octal_escapes",
    "ok_expect",
    "only_used_in_recursion",
    "op_ref",
    "option_as_ref_deref",
    "option_env_unwrap",
    "option_filter_map",
    "option_if_let_else",
    "option_map_or_none",
    "option_map_unit_fn",
    "option_option",
    "or_fun_call",
    "or_then_unwrap",
    "out_of_bounds_indexing",
    "overflow_check_conditional",
    "overly_complex_bool_expr",
    "panic",
    "panic_in_result_fn",
    "panicking_unwrap",
    "partialeq_ne_impl",
    "partialeq_to_none",
    "path_buf_push_overwrite",
    "pattern_type_mismatch",
    "positional_named_format_parameters",
    "possible_missing_comma",
    "precedence",
    "print_in_format_impl",
    "print_literal",
    "print_stderr",
    "print_stdout",
    "print_with_newline",
    "println_empty_string",
    "ptr_arg",
    "ptr_as_ptr",
    "ptr_eq",
    "ptr_offset_with_cast",
    "pub_use",
    "question_mark",
    "range_minus_one",
    "range_plus_one",
    "range_zip_with_len",
    "rc_buffer",
    "rc_clone_in_vec_init",
    "rc_mutex",
    "read_zero_byte_vec",
    "recursive_format_impl",
    "redundant_allocation",
    "redundant_clone",
    "redundant_closure",
    "redundant_closure_call",
    "redundant_closure_for_method_calls",
    "redundant_else",
    "redundant_feature_names",
    "redundant_field_names",
    "redundant_pattern",
    "redundant_pattern_matching",
    "redundant_pub_crate",
    "redundant_slicing",
    "redundant_static_lifetimes",
    "ref_binding_to_reference",
    "ref_option_ref",
    "repeat_once",
    "rest_pat_in_fully_bound_structs",
    "result_large_err",
    "result_map_or_into_option",
    "result_map_unit_fn",
    "result_unit_err",
    "return_self_not_must_use",
    "reversed_empty_ranges",
    "same_functions_in_if_condition",
    "same_item_push",
    "same_name_method",
    "search_is_some",
    "self_assignment",
    "self_named_constructors",
    "self_named_module_files",
    "semicolon_if_nothing_returned",
    "separated_literal_suffix",
    "serde_api_misuse",
    "shadow_reuse",
    "shadow_same",
    "shadow_unrelated",
    "short_circuit_statement",
    "should_implement_trait",
    "significant_drop_in_scrutinee",
    "similar_names",
    "single_char_add_str",
    "single_char_lifetime_names",
    "single_char_pattern",
    "single_component_path_imports",
    "single_element_loop",
    "single_match",
    "single_match_else",
    "size_of_in_element_count",
    "skip_while_next",
    "slow_vector_initialization",
    "stable_sort_primitive",
    "std_instead_of_alloc",
    "std_instead_of_core",
    "str_to_string",
    "string_add",
    "string_add_assign",
    "string_extend_chars",
    "string_from_utf8_as_bytes",
    "string_lit_as_bytes",
    "string_slice",
    "string_to_string",
    "strlen_on_c_strings",
    "struct_excessive_bools",
    "suboptimal_flops",
    "suspicious_arithmetic_impl",
    "suspicious_assignment_formatting",
    "suspicious_else_formatting",
    "suspicious_map",
    "suspicious_op_assign_impl",
    "suspicious_operation_groupings",
    "suspicious_splitn",
    "suspicious_to_owned",
    "suspicious_unary_op_formatting",
    "swap_ptr_to_ref",
    "tabs_in_doc_comments",
    "temporary_assignment",
    "to_digit_is_some",
    "to_string_in_format_args",
    "todo",
    "too_many_arguments",
    "too_many_lines",
    "toplevel_ref_arg",
    "trailing_empty_array",
    "trait_duplication_in_bounds",
    "transmute_bytes_to_str",
    "transmute_float_to_int",
    "transmute_int_to_bool",
    "transmute_int_to_char",
    "transmute_int_to_float",
    "transmute_num_to_bytes",
    "transmute_ptr_to_ptr",
    "transmute_ptr_to_ref",
    "transmute_undefined_repr",
    "transmutes_expressible_as_ptr_casts",
    "transmuting_null",
    "trim_split_whitespace",
    "trivial_regex",
    "trivially_copy_pass_by_ref",
    "try_err",
    "type_complexity",
    "type_repetition_in_bounds",
    "undocumented_unsafe_blocks",
    "undropped_manually_drops",
    "unicode_not_nfc",
    "unimplemented",
    "uninit_assumed_init",
    "uninit_vec",
    "unit_arg",
    "unit_cmp",
    "unit_hash",
    "unit_return_expecting_ord",
    "unnecessary_cast",
    "unnecessary_filter_map",
    "unnecessary_find_map",
    "unnecessary_fold",
    "unnecessary_join",
    "unnecessary_lazy_evaluations",
    "unnecessary_mut_passed",
    "unnecessary_operation",
    "unnecessary_owned_empty_strings",
    "unnecessary_self_imports",
    "unnecessary_sort_by",
    "unnecessary_to_owned",
    "unnecessary_unwrap",
    "unnecessary_wraps",
    "unneeded_field_pattern",
    "unneeded_wildcard_pattern",
    "unnested_or_patterns",
    "unreachable",
    "unreadable_literal",
    "unsafe_derive_deserialize",
    "unsafe_removed_from_name",
    "unseparated_literal_suffix",
    "unsound_collection_transmute",
    "unused_async",
    "unused_io_amount",
    "unused_peekable",
    "unused_rounding",
    "unused_self",
    "unused_unit",
    "unusual_byte_groupings",
    "unwrap_in_result",
    "unwrap_or_else_default",
    "unwrap_used",
    "upper_case_acronyms",
    "use_debug",
    "use_self",
    "used_underscore_binding",
    "useless_asref",
    "useless_attribute",
    "useless_conversion",
    "useless_format",
    "useless_let_if_seq",
    "useless_transmute",
    "useless_vec",
    "vec_box",
    "vec_init_then_push",
    "vec_resize_to_zero",
    "verbose_bit_mask",
    "verbose_file_reads",
    "vtable_address_comparisons",
    "while_immutable_condition",
    "while_let_loop",
    "while_let_on_iterator",
    "wildcard_dependencies",
    "wildcard_enum_match_arm",
    "wildcard_imports",
    "wildcard_in_or_patterns",
    "write_literal",
    "write_with_newline",
    "writeln_empty_string",
    "wrong_self_convention",
    "wrong_transmute",
    "zero_divided_by_zero",
    "zero_prefixed_literal",
    "zero_ptr",
    "zero_sized_map_values",
    "zst_offset",

}
