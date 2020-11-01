<?php
declare(strict_types=1);

namespace Soong\Transformer\Property;

use Soong\Contracts\Exception\PropertyTransformerException;

/**
 * PropertyTransformer to lookup a value to be returned based on an input value.
 */
class ValueLookup extends PropertyTransformerBase
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['lookup_table'] = [
            'required' => true,
            'allowed_types' => 'array',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function transform($data)
    {
        if (is_null($data)) {
            return null;
        }
        if (!is_scalar($data)) {
            throw new PropertyTransformerException(
                "ValueLookup property transformer: expected scalar value, received " .
                gettype($data)
            );
        }

        return $this->getConfigurationValue('lookup_table')[$data];
    }
}
