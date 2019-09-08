<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Document extends Model
{
    protected $table = 'mindmap_document';
    
    protected $guarded = ['id'];

    public function rootTopic() {
        return $this->morphOne('App/Topic', 'root_topic');
    }
}
