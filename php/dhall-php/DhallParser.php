<?php

namespace dawehner\DhallPhp;

use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\OptionalChoice;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\parser\CharParser;
use vektah\parser_combinator\parser\CharRangeParser;
use vektah\parser_combinator\parser\RegexParser;

class DhallParser
{
    public function __construct()
    {
        $end_of_line = new RegexParser('\\n|\\r\\n');

        //This rule matches all characters that are not:
        //
        //* not ASCII
        //* not part of a surrogate pair
        //* not a "non-character"
        $valid_non_ascii = new CharRangeParser([
            '80' => 'D7FF',
            'E00' => 'FFFD',
            '10000' => '1FFFD',
            '20000' => '2FFFD',
            '30000' => '3FFFD',
            '40000' => '4FFFD',
            '50000' => '5FFFD',
            '60000' => '6FFFD',
            '70000' => '7FFFD',
            '80000' => '8FFFD',
            '90000' => '9FFFD',
            'A0000' => 'AFFFD',
            'B0000' => 'BFFFD',
            'C0000' => 'CFFFD',
            'D0000' => 'DFFFD',
            'E0000' => 'EFFFD',
            'F0000' => 'FFFFD',
            '100000' => '10FFFD',
        ]);

        $tab = new CharParser("\t");

        $block_comment_continue = null;
        $block_comment = new Sequence([
            new CharParser('{'),
            $block_comment_continue,
        ]);;
        $block_comment_char = new Choice([
            new CharRangeParser(['20' => '7F']),
            $valid_non_ascii,
            $tab,
            $end_of_line
        ]);
        $block_comment_continue = new Choice([
            new CharParser('{'),
            $block_comment,
            $block_comment_char
        ]);
        $not_end_of_line = new Choice([
            new CharRangeParser(['20' => '7F']),
            $valid_non_ascii,
            $tab,
        ]);
        $line_comment = new Sequence([
            new CharParser('--')
            ,
            new Many($not_end_of_line)
            ,
            $end_of_line
        ]);

        $whitespace_chunk = new Choice([
            new CharParser(' '),
            $tab,
            $end_of_line,
            $line_comment,
            $block_comment,
        ]);

        $whsp = new Many($whitespace_chunk);

        $whsp1 = new Many($whitespace_chunk, 1);

        $alpha = new CharRangeParser(['41' => '5A', '61' => '7A']);

        $digit = new CharRangeParser(['30' => '39']);

        $alphanum = new Choice([$alpha, $digit]);

        $hexdig = new Choice([
            $digit,
            new CharParser('A'),
            new CharParser('B'),
            new CharParser('C'),
            new CharParser('D'),
            new CharParser('E'),
            new CharParser('F'),
        ]);

        $simple_label_first_char = new Choice([
            $alpha,
            new CharParser('_'),
        ]);
        $simple_label_next_char = new Choice([
            $alphanum,
            new CharParser('-'),
            new CharParser('/'),
            new CharParser('_'),
        ]);
        $simple_label = new Sequence([
            $simple_label_first_char,
            new Many($simple_label_next_char),
        ]);

        $quoted_label_char = new CharRangeParser([
            '20' => '5F',
            '61' => '7E',
        ]);

        $quoted_label = new Many($quoted_label_char);

        $label = new Choice([
            new Sequence([new CharParser('`'), $quoted_label, new CharParser('`')]),
            $simple_label,
        ]);

        $nonreserved_label = $label;

        $any_label = $label;

        $any_label_or_some = new Choice([$any_label, new CharParser('Some')]);

        $double_quote_escaped = new Choice([
            new CharParser(0x22),
            new CharParser(0x24),
            new CharParser(0x5C),
            new CharParser(0x2F),
            new CharParser(0x62),
            new CharParser(0x66),
            new CharParser(0x6E),
            new CharParser(0x72),
            new CharParser(0x74),
            new Sequence([new CharParser(0x75), $unicode_escape]),
        ]);

        $double_quote_chunk = new Choice([
            // todo $interpolation
            new Sequence([new CharRangeParser(['5C' => '5C'], $double_quote_escaped]));]);

        $unicode_escape = new Choice([
            $unbraced_escape,
            new Sequence([new CharParser('{'), $braced_escape, new CharParser('}')])
        ]);

        $unicode_suffix = new Choice([
            new Sequence([
                new Choice([
                    $digit,
                    new CharParser('A'),
                    new CharParser('B'),
                    new CharParser('C'),
                    new CharParser('D'),
                    new CharParser('E'),
                ]),
                new Many($hexdig, 3, 3),
            ])
            ,
            new Sequence([
                new CharParser('F')
                ,
                new Many($hexdig, 2, 2)
                ,
                new Choice([
                    $digit,
                    new CharParser('A'),
                    new CharParser('B'),
                    new CharParser('C'),
                    new CharParser('D'),
                ]),
            ]),
        ]);

        // @todo
        $unbraced_escape = new Sequence([
            new Choice([
                $digit,
                new CharParser('A'),
                new CharParser('B'),
                new CharParser('C'),
            ]),
            new Many($hexdig, 3, 3),
        ]);

        $braced_codepoint = new Many($hexdig, 3, 3);

        $braced_escape = new Sequence([new Many('0'), $braced_codepoint]);

        $double_quote_char = new Choice([
            new CharRangeParser(['20' => '21']),
            new CharRangeParser(['23' => '5B']),
            new CharRangeParser(['5D' => '7F']),
            $valid_non_ascii,
        ]);

        $double_quote_literal = new Sequence([
            new CharRangeParser(['22' => 22]),
            $double_quote_chunk,
            new CharRangeParser(['22' => 22]),
        ]);

        $escaped_quote_pair = new CharParser("'", 3, 3);
        // @todo
        $single_quote_continue = new Choice([
//            new Sequence([$interpolation, $single_quote_continue]),
            new Sequence([$escaped_quote_pair]),
        ]);

        // @todo
        $escaped_interpolation = '';

        $single_quote_char = new Choice([
            new CharRangeParser(['20' => '7F']),
            $valid_non_ascii,
            $tab,
            $end_of_line
        ]);

        $single_quote_literal = new Sequence([
            new CharParser("'", 2, 2),
            $end_of_line,
            $single_quote_continue
        ]);

        $interpolation = new Sequence([new CharParser('${'), $complete_expression, new CharParser('}')]);

        $text_literal = new Choice([
            $double_quote_literal,
            $single_quote_literal
        ]);

        $if = new RegexParser('if');
        $then = new RegexParser('then');
        $else = new RegexParser('else');
        $let = new RegexParser('let');
        $in = new RegexParser('in');
        $as = new RegexParser('as');
        $using = new RegexParser('using');
        $merge = new RegexParser('merge');
        $missing = new RegexParser('missing');
        $infinity = new RegexParser('Infinity');
        $nan = new RegexParser('NaN');
        $some = new RegexParser('Some');
        $toMap = new RegexParser('toMap');
        $assert = new RegexParser('assert');
        $forall = new Choice(['∀', new RegexParser('forall')]);
        $with = new RegexParser('with');

        $keyword = new Choice([
            $if,
            $then,
            $else,
            $let,
            $in,
            $as,
            $using,
            $merge,
            $missing,
            $infinity,
            $nan,
            $some,
            $toMap,
            $assert,
            $forall,
            $with,
        ]);

        $optional = new RegexParser('Optional');
        $text = new RegexParser('Text');
        $list = new RegexParser('List');
        $location = new RegexParser('Location');

        $bool = new RegexParser('Bool');
        $true = new RegexParser('True');
        $false = new RegexParser('False');
        $none = new RegexParser('None');
        $natural = new RegexParser('Natural');
        $integer = new RegexParser('Integer');
        $type = new RegexParser('Type');
        $kind = new RegexParser('Kind');
        $sort = new RegexParser('Sort');
        $natural_fold = new RegexParser('Natural-fold');
        $natural_build = new RegexParser('Natural-build');
        $natural_iszero = new RegexParser('Natural-isZero');
        $natural_even = new RegexParser('Natural-even');
        $natural_odd = new RegexParser('Natural-odd');
        $natural_toInteger = new RegexParser('Natural-toInteger');
        $natural_show = new RegexParser('Natural-show');
        $natural_substract = new RegexParser('Natural-substract');
        $integer_toDouble = new RegexParser('Integer-toDouble');
        $integer_show = new RegexParser('Integer-show');
        $integer_negate = new RegexParser('Integer-negate');
        $integer_clamp = new RegexParser('Integer-clamp');
        $double_show = new RegexParser('Double-show');
        $list_build = new RegexParser('List-build');
        $list_fold = new RegexParser('List-fold');
        $list_length = new RegexParser('List-length');
        $list_head = new RegexParser('List-head');
        $list_last = new RegexParser('List-last');
        $list_indexed = new RegexParser('List-indexed');
        $list_reverse = new RegexParser('List-reverse');
        $optional_fold = new RegexParser('Optional-fold');
        $optional_build = new RegexParser('Optional-build');
        $text_show = new RegexParser('Text-show');

        $combine = new Choice([
            0x2227,
            "/\\",
        ]);
        $combine_types = new Choice([
            0x2A53,
            "//\\\\",
        ]);
        $equivalent = new Choice([
            0x2261,
            "===",
        ]);
        $prefer = new Choice([
            0x2AFD,
            "//",
        ]);
        $lambda = new Choice([
            0x3BB,
            "\\",
        ]);
        $arrow = new Choice([
            0x2192,
            "->",
        ]);
        $complete = new RegexParser('::');

        $exponent = new Sequence([
            'e',
            new OptionalChoice(['+', '-']),
            new Many($digit, 1),
        ]);

        $numeric_double_literal = new Sequence([
            new OptionalChoice('+', '-'),
            new Many($digit, 1),
            new Choice([
                new Sequence('.', new Many($digit, 1), new OptionalChoice($exponent))
                ,
                $exponent,
            ]),
        ]);

        $minus_infinity_literal = new Sequence(['-', $infinity]);
        $plus_infinity_literal = $infinity;

        $double_literal = new Choice([
            $numeric_double_literal,
            $minus_infinity_literal,
            $plus_infinity_literal,
            $nan,
        ]);

        $natural_literal = new Choice([
            new Sequence(['0', 0x78, new Many($hexdig, 1)])
            ,
            new Sequence(new Choice(['1', '2', '3', '4', '5', '6', '7', '8', '9'], new Many($digit)))
            ,
            '0'
        ]);

        $integer_literal = new Sequence([
            new Choice(['+', '-'])
            ,
            $natural_literal,
        ]);

        $variable = new Sequence([
            $nonreserved_label,
            new OptionalChoice([
                new Sequence([
                    $whsp,
                    '@',
                    $whsp,
                    $natural_literal
                ]),
            ]),
        ]);


        $builtin = new Choice([
            $natural_fold,
            $natural_build,
            $natural_iszero,
            $natural_even,
            $natural_odd,
            $natural_toInteger,
            $natural_show,
            $integer_toDouble,
            $integer_show,
            $integer_negate,
            $integer_clamp,
            $natural_substract,
            $double_show,
            $list_build,
            $list_fold,
            $list_length,
            $list_head,
            $list_last,
            $list_indexed,
            $list_reverse,
            $optional_fold,
            $optional_build,
            $text_show,
            $bool,
            $true,
            $false,
            $optional,
            $none,
            $natural,
            $integer,
            $double,
            $text,
            $list,
            $kind,
            $sort,
        ]);

        $identifier = new Choice([$variable, $builtin]);

        $path_character = new CharRangeParser([
            '21' => '21',
            '24' => '27',
            '2A' => '2B',
            '2D' => '2E',
            '30' => '3B',
            '3D' => '3D',
            '40' => '5A',
            '5E' => '7A',
            '7C' => '7C',
            '7E' => '7E',
        ]);

        $quoted_path_character = new Choice([
            new CharRangeParser([
                '20' => '21',
                '23' => '2E',
                '30' => '7F',
            ]),
            $valid_non_ascii
        ]);

        $unquoted_path_component = new Many($path_character, 1);
        $quoted_path_component = new Many($quoted_path_character, 1);

        $path_component = new Sequence('/', new Choice($unquoted_path_component, 0x22, $quoted_path_component, 0x22));

        $path = new Many($path_component, 1);

        $parent_path = new Sequence('..', $path);
        $here_path = new Sequence('.', $path);
        $home_path = new Sequence('~', $path);
        $absolute_path = $path;

        $local = new Choice([
            $parent_path,
            $here_path,
            $home_path,
            $absolute_path,
        ]);

        $scheme = new RegexParser('https?');

        /** TODO $ipv6address; */

        $port = new Many($digit);

        $h16 = new Many($hexdig, 1, 4);
        $ls32 = new Choice(new Sequence($h16, ':', $h16), $ipv4address);

        $dec_octet = new Choice([
            new Sequence("25", new CharRangeParser(['30' => '35'])),
            new Sequence("2", new CharRangeParser(['30' => '34'], $digit)),
            new Sequence("1", new Many($digit, 2, 2)),
            new Sequence(new CharRangeParser(['31' => '39'], $digit)),
            $digit,
        ]);

        $domain_label = new Sequence(
            new Many($alphanum, 1),
            new Many(
                new Sequence(
                    new Many('0', 1),
                    new Many($alphanum, 1),
                )
            )
        );
        $domain = new Sequence($domain_label, new Many(
            new Sequence('.', $domain_label)
            , new RegexParser('\.?'),
        ));

        $unreserved = new Choice(
            $alphanum,
            '-',
            '.',
            '_',
            '~',
        );

        $sub_delims = new Choice(
            '!',
            '$',
            '&',
            "'",
            '*',
            '+',
            ';',
            '=',
        );

        $pct_encoded = new Sequence('%', $hexdig, $hexdig);

        $pchar = new Choice([
            $unreserved,
            $pct_encoded,
            $sub_delims,
            ':',
            '@'
        ]);
        $segment = new Many($pchar);

        $ipv4address = new Sequence($dec_octet, '.', $dec_octet, '.', $dec_octet, '.', $dec_octet);

        $ipvfuture = new Sequence([
            'v',
            new Many($hexdig, 1),
            '.',
            new Many(new Choice($unreserved, $sub_delims, ':'), 1)
        ]);

        $ip_literal = new Sequence('[', new Choice(/**$ipv6address*/ , $ipvfuture), ']');

        $user_info = new Many(new Choice($unreserved, $pct_encoded, $sub_delims, ':'));

        $url_path = new Many(new Choice($path_component, new Sequence('/', $segment));

        $host = new Choice($ip_literal, $ipv4address, $domain);

        $authority = new Sequence([
            new OptionalChoice(new Sequence($user_info, '@')),
            $host,
            new OptionalChoice(new Sequence(':', $port))
        ]);

        $query = new Many(new Choice(
            $pchar,
            '/',
            '?',
        ));

        $http_raw = new Sequence($scheme, '://', $authority, $url_path, new OptionalChoice(new Sequence('?', $query)));

        $http = new Sequence(
            $http_raw,
            new OptionalChoice(new Sequence(
                $whsp,
                $using,
                $whsp1,
                $import_expression,
            ))
        );

        $bash_environment_variable = new Sequence(
            new Choice($alpha, '_'),
            new Many(new Choice($alphanum, '_')),
        );
        /* @todo */
        $posix_environment_variable = $bash_environment_variable;

        $env = new Sequence(
            'env:',
            new Choice([
                $bash_environment_variable,
                new Sequence(0x22, $posix_environment_variable, 0x22)
            ]),
        );

        $import_type = new Choice(
            $missing,
            $local,
            $http,
            $env,
        );

        $hash = new RegexParser('sha256:', new Many($hexdig, 64, 64));
        $import_hashed = new Sequence(
            $import_type,
            new OptionalChoice(new Sequence($whsp1, $hash))
        );
        $import = new Sequence($import_hashed,
            new OptionalChoice(new Sequence($whsp, $as, $whsp1, new Choice($text, $location))),
        );

        $expression = new Choice(
            new Sequence($lambda, $whsp, '(', $whsp, $nonreserved_label, $whsp, ':', $whsp1, $expression, $whsp, ')',
                $whsp, $arrow, $whsp, $expression),
            new Sequence($if, $whsp1, $expression, $whsp, $then, $whsp1, $expression, $whsp, $else, $whsp1,
                $expression),
            new Sequence(new Many($let_binding, 1), $in, $whsp1, $expression),
            new Sequence($forall, $whsp, '(', $whsp, $nonreserved_label, $whsp, ':', $whsp1, $expression, $whsp, ')',
                $whsp, $arrow, $whsp, $expression,
            ),
            new Sequence($operator_expression, $whsp, $arrow, $whsp, $expression),
            $with_expression,
            new Sequence($merge, $whsp1, $import_expression, $whsp1, $import_expression, $whsp, ':', $whsp1,
                $application_expression),
            $empty_list_literal,
            new Sequence($toMap, $whsp1, $import_expression, $whsp, ':', $whsp1, $application_expression),
            new Sequence($assert, $whsp, ':', $expression),
            $annotated_expression,
        );

        $annotated_expression = new Sequence(
            $operator_expression,
            new OptionalChoice(new Sequence($whsp, ':', $whsp1, $expression))
        );

        $let_binding = new Sequence($let, $whsp, $nonreserved_label, $whsp,
            new OptionalChoice(new Sequence(':', $whsp1, $expression, $whsp)), '=', $whsp, $expression, $whsp);

        $empty_list_literal = new Sequence(
            '[', $whsp, new OptionalChoice(new Sequence(',', $whsp)), ']',
            $whsp, ':', $whsp1, $application_expression
        );

        $with_expression = new Sequence(
            $import_expression,
            new Many(new Sequence($whsp1, $with, $whsp1, $with_clause), 1)
        );

        $operator_expression = $equivalent_expression;


        $with_clause = new Sequence(
            $any_label_or_some,
            new Many(new Sequence(
                $whsp, '.', $whsp, $any_label_or_some
            )),
            $whsp, '=', $whsp, $operator_expression,
        );

        $record_literal_normal_entry = new Sequence(
            new Many(new Sequence($whsp, '.', $whsp, $any_label_or_some)),
            $whsp, '=', $whsp, $expression,
        );

        $record_literal_entry = new Sequence(
            $any_label_or_some,
            new OptionalChoice($record_literal_normal_entry)
        );

        $non_empty_record_literal =
            new Sequence($record_literal_entry, new Many(
                new Sequence($whsp, ',', $whsp, $record_literal_entry)
            ));

        $record_type_entry = new Sequence(
            $any_label_or_some,
            $whsp,
            ':' .,
            $whsp1,
            $expression,
        );

        $non_empty_record_type = new Sequence(
            $record_type_entry,
            new Many(new Sequence($whsp, ',', $whsp, $record_type_entry)),
        );

        $non_empty_record_type_or_literal = new Choice(
            $non_empty_record_type, $non_empty_record_literal
        );

        $empty_record_literal = new CharParser('=');

        $record_type_or_literal = new Choice(
            $empty_record_literal,
            new OptionalChoice($non_empty_record_type_or_literal)
        );


        $union_type_entry = new Sequence(
            $any_label_or_some,
            new OptionalChoice(new Sequence($whsp, ':', $whsp1, $expression)),
        );

        $union_type = new OptionalChoice(
            new Sequence($union_type_entry, new Many(new Sequence(
                $whsp, '|', $whsp, $union_type_entry
            )))
        );

        $primitive_expression = new Choice(
            $double_literal,
            $natural_literal,
            $integer_literal,
            $text_literal,
            new Sequence('{', $whsp, new OptionalChoice(new Sequence(',', $whsp), $record_type_or_literal, $whsp, '}')),
            new Sequence('<', $whsp, new OptionalChoice(new Sequence('|', $whsp), $union_type, $whsp, '>')),
            $non_empty_list_literal,
            $identifier,
            new Sequence('(', $complete_expression, ')')
        );

        $type_selector = new Sequence('(', $whsp, $expression, $whsp, ')');

        $labels = new Sequence('{', $whsp,
            new OptionalChoice(new Sequence($any_label_or_some, $whsp,
                new Many(new Sequence(',', $whsp, $any_label_or_some))
            )),
            '}'
        );

        $selector = new Choice($any_label, $labels, $type_selector);

        $selector_expression = new Sequence(
            $primitive_expression,
            new Many(new Sequence($whsp, '.', $whsp, $selector))
        );

        $completion_expression = new Sequence(
            $selector_expression,
            new OptionalChoice(new Sequence(
                $whsp,
                $complete,
                $whsp,
                $selector_expression,
            )),
        );

        $import_expression = new Choice(
            $import, $complete_expression
        );

        $first_application_expression = new Choice(
            new Sequence($merge, $whsp1, $import_expression, $whsp1, $import_expression),
            new Sequence($some, $whsp1, $import_expression),
            new Sequence($toMap, $whsp1, $import_expression),
            $import_expression,
        );

        $application_expression = new Sequence(
            $first_application_expression,
            new Many(new Sequence($whsp1, $import_expression))
        );

        $not_equal_expression = new Sequence($application_expression,
            new Many(new Sequence($whsp, '!==', $whsp, $application_expression))
        );

        $equal_expression = new Sequence($not_equal_expression,
            new Many(new Sequence($whsp, '==', $whsp, $not_equal_expression))
        );

        $times_expression = new Sequence($equal_expression,
            new Many(new Sequence($whsp, '*', $whsp, $equivalent_expression))
        );

        $combine_types_expression = new Sequence($times_expression,
            new Many(new Sequence($whsp, $combine_types, $whsp, $times_expression))
        );

        $prefer_expression = new Sequence($combine_types_expression,
            new Many(new Sequence($whsp, $prefer, $whsp, $combine_types_expression))
        );

        $combine_expression = new Sequence($prefer_expression,
            new Many(new Sequence($whsp, $combine, $whsp, $prefer_expression))
        );

        $and_expression = new Sequence($combine_expression,
            new Many(new Sequence($whsp, '&&', $whsp, $combine_expression))
        );

        $list_append_expression = new Sequence($and_expression,
            new Many(new Sequence($whsp, '#', $whsp, $and_expression))
        );
        $text_append_expression = new Sequence(
            $list_append_expression,
            new Many(new Sequence($whsp, '++', $whsp, $list_append_expression)),
        );
        $plus_expression = new Sequence($text_append_expression,
            new Many(new Sequence($whsp, '+', $whsp1, $text_append_expression))
        );
        $or_expression = new Sequence(
            $plus_expression,
            new Many(new Sequence($whsp, '||', $whsp, $plus_expression))
        );
        $import_alt_expression = new Sequence($or_expression,)
        $equivalent_expression = new Sequence(
            $import_alt_expression
        );



        $non_empty_list_literal = new Sequence(
            '[', $whsp, new OptionalChoice(new Sequence(',', $whsp)),
            $expression,
            $whsp,
            new Many(new Sequence(',', $whsp, $expression, $whsp)),
            ']'
        );
        $complete_expression = new Sequence($whsp, $expression, $whsp);
    }
}