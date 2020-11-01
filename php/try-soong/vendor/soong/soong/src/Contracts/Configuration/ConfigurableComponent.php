<?php
declare(strict_types=1);

namespace Soong\Contracts\Configuration;

interface ConfigurableComponent
{

    /**
     * Retrieve a configuration value for a named option.
     *
     * @param string $optionName
     *   Name of the option to retrieve.
     *
     * @return mixed|null
     *   Retrieved value, or NULL if unset.
     */
    public function getConfigurationValue(string $optionName);

    /**
     * Retrieve a list of all available configuration values.
     *
     * @return iterable
     *   Option values keyed by names.
     */
    public function getAllConfigurationValues() : iterable;
}
