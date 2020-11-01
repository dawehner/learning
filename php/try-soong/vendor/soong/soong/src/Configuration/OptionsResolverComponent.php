<?php
declare(strict_types=1);

namespace Soong\Configuration;

use Soong\Contracts\Configuration\ConfigurableComponent;
use Symfony\Component\OptionsResolver\OptionsResolver;

abstract class OptionsResolverComponent implements ConfigurableComponent
{

    /**
     * Resolved configuration for the component.
     *
     * @var array $configuration
     *   Configuration values keyed by configuration name.
     */
    protected $configuration = [];

    /**
     * Use Symfony OptionsResolver as our default configuration manager.
     *
     * @var OptionsResolver $resolver
     */
    protected $resolver;

    public function __construct(array $configuration)
    {
        // Setup and validate configuration.
        $this->resolver = new OptionsResolver();
        foreach ($this->optionDefinitions() as $optionName => $optionDefinition) {
            if (isset($optionDefinition['default_value'])) {
                $this->resolver->setDefault($optionName, $optionDefinition['default_value']);
            } else {
                $this->resolver->setDefined($optionName);
            }
            if (isset($optionDefinition['required']) && $optionDefinition['required']) {
                $this->resolver->setRequired($optionName);
            }
            if (isset($optionDefinition['allowed_types'])) {
                $this->resolver->setAllowedTypes($optionName, $optionDefinition['allowed_types']);
            }
            if (isset($optionDefinition['allowed_values'])) {
                $this->resolver->setAllowedValues($optionName, $optionDefinition['allowed_values']);
            }
        }
        $this->configuration = $this->resolver->resolve($configuration);
    }

    /**
     * List of definition arrays for each option supported by the component.
     *
     * @return array
     *   Keyed by option name, each element is an array with these contents:
     *     default_value - default value, if any
     *     required - true if the option is required, false if optional.
     *     allowed_types - types of values allowed for the option.
     *
     */
    protected function optionDefinitions() : array
    {
        return [];
    }

    /**
     * @inheritdoc
     */
    public function getConfigurationValue(string $optionName)
    {
        return $this->configuration[$optionName] ?? null;
    }

    /**
     * @inheritdoc
     */
    public function getAllConfigurationValues(): iterable
    {
        return $this->configuration;
    }
}
