<?php
declare(strict_types=1);

namespace Soong\Transformer\Property;

use Soong\Contracts\Exception\PropertyTransformerException;

/**
 * PropertyTransformer to multiply the extracted data value by 2.
 */
class Double extends PropertyTransformerBase
{

    /**
     * @inheritdoc
     */
    public function transform($data)
    {
        if (is_null($data)) {
            return null;
        }
        if (!is_numeric($data)) {
            throw new PropertyTransformerException(
                "Double property transformer: expected numeric value, received " .
                gettype($data)
            );
        }
        return 2 * $data;
    }
}
