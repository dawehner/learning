<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Topic extends Model
{
    protected $table = 'mindmap_topic';

    protected $guarded = ['id'];
    
    public function parent() {
        return $this->belongsTo('App/Topic', 'parent_id');
    }
    
    public function root() {
        return $this->belongsTo('App/Topic', 'root_id');
    }
    
    public function children() {
        return $this->hasMany('App/Topic', 'parent_id');
    }
}
