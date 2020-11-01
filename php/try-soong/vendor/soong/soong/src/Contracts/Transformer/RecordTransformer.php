<?php
declare(strict_types=1);

namespace Soong\Contracts\Transformer;

use Soong\Contracts\Configuration\ConfigurableComponent;
use Soong\Contracts\Data\Record;

/**
 * Accept a data record and turn it into another data record.
 */
interface RecordTransformer extends ConfigurableComponent
{

    /**
     * Accept a data record and turn it into another data record.
     *
     * @throws \Soong\Contracts\Exception\RecordTransformerException
     *
     * @param Record $sourceData
     *   Record containing incoming data properties to be transformed.
     * @param Record $resultData
     *   Record being populated with transformed properties.
     *
     * @return \Soong\Contracts\Data\Record
     *   The transformed record.
     */
    public function transform(Record $sourceData, Record $resultData) : Record;
}
