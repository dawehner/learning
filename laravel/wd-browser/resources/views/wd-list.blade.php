<div>
    @foreach ($results as $entry)
        <x-wd-entry :id="$entry->id" :lang="$entry->lang" :label="$entry->label" :url="$entry->wiki_url"
                    :description="$entry->description"/>
    @endforeach
</div>
