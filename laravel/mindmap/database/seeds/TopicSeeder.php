<?php

use App\Document;
use App\Topic;
use Illuminate\Database\Seeder;
use Illuminate\Support\Facades\DB;

class TopicSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */
    public function run()
    {
        DB::table('mindmap_topic')->delete();

        $topic = Topic::create([
            'title' => 'root',
        ]);

        Document::create([
            'title' => 'My initial mindmap',
        ])->save();
        Document::create([
            'title' => 'My other mindmap',
        ])->save();
    }
}
