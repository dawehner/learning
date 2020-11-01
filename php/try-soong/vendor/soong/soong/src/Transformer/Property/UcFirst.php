<?php
declare(strict_types=1);

namespace Soong\Transformer\Property;

use Soong\Contracts\Exception\PropertyTransformerException;

/**
 * PropertyTransformer to uppercase the first letter of the extracted data.
 */
class UcFirst extends PropertyTransformerBase
{

    /**
     * @inheritdoc
     */
    public function transform($data) : ?string
    {
        if (is_null($data)) {
            return null;
        }
        if (!is_string($data)) {
            throw new PropertyTransformerException(
                "UcFirst property transformer: expected string value, received " .
                gettype($data)
            );
        }

        return ucfirst($data);
    }
}
