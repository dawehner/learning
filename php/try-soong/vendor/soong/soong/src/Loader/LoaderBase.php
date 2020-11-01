<?php
declare(strict_types=1);

namespace Soong\Loader;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\Loader\Loader;

/**
 * Common implementation details for loaders.
 */
abstract class LoaderBase extends OptionsResolverComponent implements Loader
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['properties'] = [
            'required' => true,
            'default_value' => [],
            'allowed_types' => 'array',
        ];
        $options['key_properties'] = [
            'required' => true,
            'default_value' => [],
            'allowed_types' => 'array',
        ];
        return $options;
    }

    /**
     * @inheritdoc
     */
    public function getProperties(): array
    {
        return $this->getConfigurationValue('properties') ?? [];
    }

    /**
     * @inheritdoc
     */
    public function getKeyProperties(): array
    {
        return $this->getConfigurationValue('key_properties') ?? [];
    }
}
