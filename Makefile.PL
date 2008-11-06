use inc::Module::Install;
name 'tTemplate';
requires( perl => '5.8.3' );

all_from 'lib/tTemplate.pm';

build_requires 'Test::More';
use_test_base;
auto_include_deps;
author_tests('xt');
WriteAll;
