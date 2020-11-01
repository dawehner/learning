<?php
declare(strict_types=1);

namespace Soong\Contracts\Task;

use Soong\Contracts\Extractor\Extractor;
use Soong\Contracts\KeyMap\KeyMap;
use Soong\Contracts\Loader\Loader;

/**
 * Tasks for performing migration operations.
 */
interface EtlTask extends Task
{

    /**
     * Retrieves the configured extractor for this task, if any.
     *
     * @return Extractor
     *   The extractor, or NULL if none.
     */
    public function getExtractor() : ?Extractor;

    /**
     * Retrieves the configured loader for this task, if any.
     *
     * @return Loader
     *   The loader, or NULL if none.
     */
    public function getLoader() : ?Loader;

    /**
     * Retrieves the configured keymap for this task, if any.
     *
     * @return KeyMap
     *   The keymap, or NULL if none.
     */
    public function getKeyMap() : ?KeyMap;
}
