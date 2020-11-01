<?php
declare(strict_types=1);

namespace Soong\Console\Command;

use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

/**
 * Implementation of the "rollback" console command.
 */
class RollbackCommand extends EtlCommand
{

    /**
     * @inheritdoc
     */
    protected function configure()
    {
        $this->setName("rollback")
          ->setDescription("Remove migrated data")
          ->setDefinition([
              $this->tasksArgument(),
              $this->directoryOption(),
              $this->limitOption(),
          ])
          ->setHelp(<<<EOT
The <info>rollback</info> command does things and stuff
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
        $options = [
            'limit' => $input->getOption('limit'),
        ];
        foreach ($input->getArgument('tasks') as $id) {
            if ($task = $this->pipeline->getTask($id)) {
                $output->writeln("<info>Executing $id</info>");
                $task->execute('rollback', $options);
            } else {
                $output->writeln("<error>$id not found</error>");
            }
        }
    }
}
