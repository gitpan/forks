package
    forks::shared::attributes; #hide from PAUSE
$VERSION = '0.27';
    
use Attribute::Handlers;


#package
#    Attribute::Handlers;
#    
#$Attribute::Handlers::builtin =
#    $Attribute::Handlers::builtin = qr/lvalue|method|locked|unique/;


package 
    UNIVERSAL; #hide from PAUSE

# Overload 'shared' attribute

sub shared : ATTR(VAR) {
    my ($package, $symbol, $referent, $attr, $data, $phase) = @_;
    $data = [ $data ] unless ref $data eq 'ARRAY';
    threads::shared::_share( $referent );
}

# Special case for perl 5.9.0 and later source filter, for 'shared' attribute

sub Forks_shared : ATTR(VAR) {
    my ($package, $symbol, $referent, $attr, $data, $phase) = @_;
    $data = [ $data ] unless ref $data eq 'ARRAY';
    threads::shared::_share( $referent );
}

1;
