<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;

class PseudoSelector extends Selector
{
    /** @var string */
    private $name;

    /** @var string */
    private $expression;

    public function __construct($name, $expression = null) {
        $this->name = $name;
        $this->expression = $expression;
    }

    /**
     * @return string
     */
    public function getExpression()
    {
        return $this->expression;
    }

    /**
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }

    public function toCss()
    {
        $expression_css = $this->expression ? "($this->expression)" : '';

        return ":$this->name$expression_css";
    }

    public function __toString()
    {
        $expression = $this->expression ? ", $this->expression" : '';
        return 'Pseudo(' . $this->name . $expression . ')';
    }

    public function define() {
        $object = new CssObject();

        switch ($this->name) {
            case 'lang':
                $object->attributes['lang'] = $this->expression;
                break;
            case 'enabled':
                unset($object->attributes['enabled']);
                break;
            case 'disabled':
                $object->attributes['disabled'] = true;
                break;
            case 'checked':
                $object->attributes['checked'] = true;
                break;
            case 'root':
                $object->isRoot = true;
                break;
        }

        return $object;
    }

    public function matchesObject(CssObject $object)
    {
        switch($this->name) {
            case 'link':
                return $object->element === 'a';
            case 'lang':
                if (!isset($object->attributes['lang'])) {
                    return false;
                }

                if ($object->attributes['lang'] === $this->expression) {
                    return true;
                }

                return substr($object->attributes['lang'], 0, strlen($this->expression) + 1) === "$this->expression-";
            case 'enabled':
                return !isset($object->attributes['disabled']);
            case 'disabled':
                return isset($object->attributes['disabled']);
            case 'checked':
                return isset($object->attributes['checked']);
            case 'root':
                return $object->isRoot;
            case 'not':
                // This is a special case when a not is inside another not, and is invalid.
                return true;
        }

        return false;
    }
}
