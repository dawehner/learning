<?php
declare(strict_types=1);

namespace Soong\Contracts\Data;

/**
 * Collection of named data properties.
 */
interface Record
{

    /**
     * Fetch the list of named properties as an associative array.
     *
     * @return array
     *   Associative array of property values, keyed by property name.
     */
    public function toArray() : array;

    /**
     * Set a property from an existing Property object.
     *
     * @param string $propertyName
     *   Name of the property to set.
     * @param mixed $propertyValue
     *   Property value to set.
     */
    public function setPropertyValue(string $propertyName, $propertyValue) : void;

    /**
     * Retrieve a property value as a Property.
     *
     * @param string $propertyName
     *   Name of the property to get.
     *
     * @return mixed
     *   Value of the property.
     */
    public function getPropertyValue(string $propertyName);

    /**
     * Does the named property exist in this record?
     *
     * @param string $propertyName
     *   Name of the property to get.
     *
     * @return bool
     *   TRUE if the property exists (even if its value is NULL), FALSE otherwise.
     */
    public function propertyExists(string $propertyName) : bool;
}
