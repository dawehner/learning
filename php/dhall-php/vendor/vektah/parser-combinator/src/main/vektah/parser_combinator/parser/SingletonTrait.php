<?php


namespace vektah\parser_combinator\parser;

trait SingletonTrait {
    private static $instance;

    public static function instance() {
        if (!self::$instance) {
            self::$instance = new static();
        }

        return self::$instance;
    }
}
