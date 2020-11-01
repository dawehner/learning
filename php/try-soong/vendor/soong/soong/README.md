# Soong\Soong

[![Latest Version on Packagist][ico-version]][link-packagist]
[![Software License][ico-license]](LICENSE.md)
[![Coverage Status][ico-scrutinizer]][link-scrutinizer]
[![Quality Score][ico-code-quality]][link-code-quality]
[![Total Downloads][ico-downloads]][link-downloads]

Soong is a framework for building robust Extract-Transform-Load (ETL) applications for performing data migration. It is designed to be record-oriented and configuration-driven - most applications will require little or no custom PHP code, and tools can easily customize (or create) data migration processes implemented by Soong.

Documentation is at https://soong-etl.readthedocs.io/.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html). For major version 0, we will increment the minor version for backward-incompatible changes. At this pre-release point, the interfaces are still changing regularly - if you develop any applications using Soong be sure to pin them to the minor release.

Right now soong/soong is standalone, containing all components developed for soong so far as well as sample migrations (see **Demos** below). Once the interfaces are stable, the plan is to break out component implementations into separate smaller libraries - soong/soong itself may end up containing only the interfaces (contracts), or (more likely) also basic implementations for the most widely-needed components. Certainly, specialized components like the DBAL integration will end up in their own repos.

## Install

Soong is best installed using [Composer](https://getcomposer.org/). Since as noted above while in major version 0 minor versions will contain backward-incompatible changes, and at this point the interfaces are still changing regularly, if you develop any applications using Soong be sure to pin them to the minor release you implemented them with. E.g., "~0.5.0" which will get the latest release with major.minor version 0.5 and prevent updating to a 0.6.x release which may break your application.

``` bash
$ composer require soong/soong "~0.5.0"
```

## Change log

Please see [CHANGELOG](docs/CHANGELOG.md) for more information on what has changed recently.

## Contributing

There's still a lot of refinement to be done to Soong - this is your opportunity to get involved with a new framework (and community) on the ground floor! As mentioned above, the plan is ultimately to break out components into small well-contained libraries - these will be excellent opportunities to get your feet wet maintaining your own open-source project.

Please see [CONTRIBUTING](docs/CONTRIBUTING.md) and [CODE_OF_CONDUCT](docs/CODE_OF_CONDUCT.md) for details.

## Security

If you discover any security related issues, please email `soong@virtuoso-performance.com` instead of using the issue tracker.

## Demos

To setup for demoing Soong ETL:

1. Create an empty database for testing.
1. Import `data/extractsource.sql` to the database (table to be populated by the first demo).
1. Import `data/beer.sql` to the database (tables to be populated for the second demo).
1. Edit each of the files in `config/` - where indicated, replace the sample credentials with those for the test database.

Demo 1:

1. Execute `bin/soong migrate arraytosql`
1. Look at the `extractsource` table to see the data populated, and that the ids have been assigned consecutively.
1. Look at the `map_arraytosql` table to see the mapping from source to destination keys.
1. Execute `bin/soong migrate sqltocsv`
1. Observe CSV data output to the terminal with configured transformations applied.
1. Execute `bin/soong rollback arraytosql`
1. Observe that the `extractsource` and `map_arraytosql` tables are now empty.

Demo 2:

1. Execute `bin/soong migrate beertopics`
1. Observe the `beer_terms` table is populated from CSV data - in particular, see how the 'red ale' reference to its 'ale' parent has been converted to the numeric ID assigned to the 'ale' row in the database.
1. Execute `bin/soong migrate beeraccounts`
1. Observe the `beer_users` table - in particular, see how the `ValueLookup` transformer converted the boolean values in the `pro` column to strings in the `taster` column.
1. Execute `bin/soong migrate beercontent`
1. Observe the `beer` table - in particular, see how the relationships to users/accounts was maintained even though the IDs for the users changed (also see the `map_beeraccounts` table).
1. Execute `bin/soong rollback beercontent`
1. Observe how the `beer` and `map_beercontent` tables are now empty.

## Credits

- [Mike Ryan][link-author]
- [All Contributors][link-contributors]

## License

The MIT License (MIT). Please see [License File](LICENSE.md) for more information.

[ico-version]: https://img.shields.io/packagist/v/soong/soong.svg?style=flat-square
[ico-license]: https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat-square
[ico-travis]: https://img.shields.io/travis/soong/soong/master.svg?style=flat-square
[ico-scrutinizer]: https://img.shields.io/scrutinizer/coverage/gl/soong/soongetl/soong.svg?style=flat-square
[ico-code-quality]: https://img.shields.io/scrutinizer/quality/gl/soong/soongetl/soong.svg?style=flat-square
[ico-downloads]: https://img.shields.io/packagist/dt/soong/soong.svg?style=flat-square

[link-packagist]: https://packagist.org/packages/soong/soong
[link-scrutinizer]: https://scrutinizer-ci.com/gl/soong/soongetl/soong/code-structure
[link-code-quality]: https://scrutinizer-ci.com/gl/soong/soongetl/soong/
[link-downloads]: https://packagist.org/packages/soong/soong
[link-author]: https://gitlab.com/mikeryan
[link-contributors]: ../../contributors
