<?php
declare(strict_types=1);

namespace Soong\Transformer\Property;

use Soong\Configuration\OptionsResolverComponent;
use Soong\Contracts\Transformer\PropertyTransformer;

/**
 * Define common options for most property transformers.
 */
abstract class PropertyTransformerBase extends OptionsResolverComponent implements PropertyTransformer
{

    /**
     * @inheritdoc
     */
    protected function optionDefinitions(): array
    {
        $options = parent::optionDefinitions();
        $options['pipeline'] = [
            'required' => true,
            'allowed_types' => 'Soong\Contracts\Task\TaskPipeline',
        ];
        return $options;
    }
}
