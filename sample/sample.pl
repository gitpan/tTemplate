binmode STDERR,":encoding(utf8)";
use strict;
use warnings;
use utf8;
use lib './lib';
use Encode;
use tTemplate;

my $infile = "t/test.html";

my $tmpl = tTemplate->newFromFile($infile,'utf8');
	## if you want catch load error, use eval and $@ ##

my $param = {
	text=>"this is text.",
	index=>"0",
	hash=>{ a => "深い階層にあるパラメータです", },
	tlist =>[
		"これはutf8フラグのついたテキストです",
		Encode::encode('cp932',"これはutf8フラグのないテキストです"),
	],
	htmltest => "日本語ABC<>&\"';[\n]",
	
	varlist =>[
		undef
		,'',' ','0E0'
		,0,1,2,3
		,0.0001
		,[1,2,3]
		,{ a=>1,b=>2,c=>3}
		,$tmpl
	],

	true => 1,

	testif =>[ (0..5)],
};

$tmpl->param_encoding('cp932');
$tmpl->filter_default('html');

my $fh;
open($fh,">","out1.html");
$tmpl->print($param,$fh,'utf8');
close($fh);

# print Encode::encode('utf8',$tmpl->toString($param));
