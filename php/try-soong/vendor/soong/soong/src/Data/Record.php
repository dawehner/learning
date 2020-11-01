<?php
declare(strict_types=1);

namespace Soong\Data;

/**
 * Basic implementation of data records as arrays.
 */
class Record implements \Soong\Contracts\Data\Record
{

    /**
     * @internal
     *
     * Array of data properties, keyed by property name.
     *
     * @var mixed $data[]
     */
    protected $data = [];

    /**
     * Create a record populated with a set of named data properties.
     *
     * @param array $data
     *   Associative array of property values, keyed by property name.
     */
    public function __construct($data = [])
    {
        foreach ($data as $propertyName => $propertyValue) {
            $this->setPropertyValue($propertyName, $propertyValue);
        }
    }

    /**
     * @inheritdoc
     */
    public function setPropertyValue(string $propertyName, $propertyValue) : void
    {
        $this->data[$propertyName] = $propertyValue;
    }

    /**
     * @inheritdoc
     */
    public function getPropertyValue(string $propertyName)
    {
        return array_key_exists($propertyName, $this->data) ? $this->data[$propertyName] : null;
    }

    /**
     * @inheritdoc
     */
    public function toArray() : array
    {
        return $this->data;
    }

    /**
     * @inheritdoc
     */
    public function propertyExists(string $propertyName): bool
    {
        return array_key_exists($propertyName, $this->data);
    }
}
