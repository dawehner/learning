<?php
declare(strict_types=1);

namespace Soong\Console\Command;

use Soong\Contracts\Task\EtlTask;
use Soong\Task\Task;
use Symfony\Component\Console\Helper\Table;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

/**
 * Implementation of the console "status" command.
 */
class StatusCommand extends EtlCommand
{

    /**
     * @inheritdoc
     */
    protected function configure()
    {
        $this->setName("status")
          ->setDescription("Report status of Soong tasks")
          ->setDefinition([
            $this->tasksArgument(false),
            $this->directoryOption(),
          ])
          ->setHelp(<<<EOT
The <info>status</info> provides info on available Soong tasks
EOT
          );
    }

    /**
     * @inheritdoc
     */
    protected function execute(InputInterface $input, OutputInterface $output)
    {
        /** @var string[] $directoryNames */
        $directoryNames = $input->getOption('directory');
        $this->loadConfiguration($directoryNames);
        $table = new Table($output);
        $table->setHeaders(['Task', 'Total', 'Processed', 'Unprocessed']);
        if (empty($taskList = $input->getArgument('tasks'))) {
            $taskList = array_keys($this->pipeline->getAllTasks());
        }
        foreach ($taskList as $id) {
            if ($task = $this->pipeline->getTask($id)) {
                $total = $processed = $unprocessed = 'N/A';
                if ($task instanceof EtlTask) {
                    $extractor = $task->getExtractor();
                    if ($extractor instanceof \Countable) {
                        $total = $extractor->count();
                    }
                    $keyMap = $task->getKeyMap();
                    if ($keyMap instanceof \Countable) {
                        $processed = $keyMap->count();
                    }
                    if (is_int($total) && is_int($processed)) {
                        $unprocessed = $total - $processed;
                    }
                }
                $table->addRow([$id, $total, $processed, $unprocessed]);
            } else {
                $output->writeln("<error>$id not found</error>");
            }
        }
        $table->render();
    }
}
