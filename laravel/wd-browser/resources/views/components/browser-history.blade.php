<h3>History</h3>
<ul>
    @foreach (array_unique($history) as $path)
        <li>
            @if (Str::startsWith($path, '/entity/Q'))
                @include('wd-link', ['ref' => str_replace('/entity/', '', $path), 'title' => $path])
            @else
                {{$path}}
            @endif
        </li>
    @endforeach
</ul>
