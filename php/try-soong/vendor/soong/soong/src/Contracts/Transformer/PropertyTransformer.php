<?php
declare(strict_types=1);

namespace Soong\Contracts\Transformer;

use Soong\Contracts\Configuration\ConfigurableComponent;

/**
 * Accept a data property and turn it into another data property.
 */
interface PropertyTransformer extends ConfigurableComponent
{

    /**
     * Accept a data property and turn it into another data property.
     *
     * @param mixed $data
     *   Property containing data to be transformed.
     *
     * @throws \Soong\Contracts\Exception\PropertyTransformerException
     *
     * @return mixed
     *   The transformed data.
     */
    public function transform($data);
}
