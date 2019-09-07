<?php

use App\Document;
use Illuminate\Database\Seeder;
use Illuminate\Support\Facades\DB;

class DocumentSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */
    public function run()
    {
        DB::table('mindmap_document')->delete();

        Document::create([
            'title' => 'My initial mindmap',
        ])->save();
        Document::create([
            'title' => 'My other mindmap',
        ])->save();
    }
}
