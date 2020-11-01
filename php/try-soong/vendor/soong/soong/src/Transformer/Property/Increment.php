<?php
declare(strict_types=1);

namespace Soong\Transformer\Property;

use Soong\Contracts\Exception\PropertyTransformerException;

/**
 * PropertyTransformer to add 1 to the extracted data.
 */
class Increment extends PropertyTransformerBase
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
                "Increment property transformer: expected numeric value, received " .
                gettype($data)
            );
        }
        return $data + 1;
    }
}
