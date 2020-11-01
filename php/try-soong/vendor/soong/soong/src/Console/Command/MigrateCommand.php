<?php
declare(strict_types=1);

namespace Soong\Console\Command;

use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

/**
 * Implementation of the "migrate" console command.
 */
class MigrateCommand extends EtlCommand
{

    /**
     * @inheritdoc
     */
    protected function configure()
    {
        $this->setName("migrate")
            ->setDescription("Migrate data from one place to another")
            ->setDefinition([
                $this->tasksArgument(),
                $this->directoryOption(),
                $this->selectOption(),
                $this->limitOption(),
            ])
            ->setHelp(<<<EOT
The <info>migrate</info> command executes a Soong migration task
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
        $options = [
            'select' => $input->getOption('select'),
            'limit' => $input->getOption('limit'),
        ];
        $this->loadConfiguration($directoryNames, $options);
        foreach ($input->getArgument('tasks') as $id) {
            if ($task = $this->pipeline->getTask($id)) {
                $output->writeln("<info>Executing $id</info>");
                $task->execute('migrate', $options);
            } else {
                $output->writeln("<error>$id not found</error>");
            }
        }
    }
}
