<?php
declare(strict_types=1);

namespace Soong\Loader;

use Soong\Contracts\Data\Record;

/**
 * Real dumb demo of a simple loader.
 *
 * @todo: Use league/csv
 */
class Csv extends LoaderBase
{

    /**
     * @internal
     *
     * Counter of records loaded, to use as a unique destination ID.
     *
     * @var int $counter
     */
    protected $counter = 0;

    /**
     * @inheritdoc
     */
    public function load(Record $data) : void
    {
        // @todo Should have a configuration option for this.
        $data->setPropertyValue(
            array_keys($this->getConfigurationValue('key_properties'))[0],
            $this->counter++
        );
        $properties = $data->toArray();
        if (count($properties) > 1) {
            if ($this->counter == 1) {
                print implode(',', $this->getConfigurationValue('properties')) . "\n";
            }
            print implode(',', $properties) . "\n";
        }
    }

    /**
     * @inheritdoc
     */
    public function delete(array $key) : void
    {
        // @todo not supported
    }
}
