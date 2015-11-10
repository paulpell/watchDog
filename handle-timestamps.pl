
use strict;
use warnings;

sub file_unique_timestamps ()
{
  
  my %timestamps = ();
  
  while (<>)
  {
    s/\r//;
    if ( m/;(\d+);/ )
    {
      my $tstamp = $1;
      if ( ! defined ($timestamps{$tstamp}) )
      {
        $timestamps{$tstamp} = 0;
        print;
      }
    }
    else # eg. header line
    {
      print;
    }
  }
}

sub unify_all_files ()
{
  print "to come soon"
}

my $arg = shift;

if ( "file" eq $arg ) { file_unique_timestamps() }
elsif ( "all"  eq $arg ) { unify_all_files() }
else { print "ERROR: Bad argument"; }
