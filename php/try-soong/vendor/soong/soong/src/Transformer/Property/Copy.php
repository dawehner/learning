<?php
declare(strict_types=1);

namespace Soong\Transformer\Property;

/**
 * PropertyTransformer to simply copy extracted data to the destination.
 */
class Copy extends PropertyTransformerBase
{

    /**
     * @inheritdoc
     */
    public function transform($data)
    {
        // Properties are immutable, so it's safe to return directly.
        return $data;
    }
}
