# Clock

An implementation of the proposed PSR-20 clock-interface

[![Total Downloads](http://poser.pugx.org/stella-maris/clock/downloads)](https://packagist.org/packages/stella-maris/clock)
[![Latest Stable Version](http://poser.pugx.org/stella-maris/clock/v)](https://packagist.org/packages/stella-maris/clock)
[![Latest Unstable Version](http://poser.pugx.org/stella-maris/clock/v/unstable)](https://packagist.org/packages/stella-maris/clock)

[![pipeline status](https://gitlab.com/stella-maris/clock/badges/main/pipeline.svg)](https://gitlab.com/stella-maris/clock/-/commits/main)

## Installation

```bash
composer require stella-maris/clock
```

## Usage

This interface allows one to inject one of the implemntations that provide the 
clock-interface.

```php
use StellaMaris/Clock/CLockInterface;

final class PastChecker
{
    public function __construct(private ClockInterface $clock) {}
    
    public function hasDateTimeAlreadyPassed(DateTimeImmutable $item): bool
    {
        return $item < $this->clock->now();
    }
}
```

## Why

Within the Framework Interoperability Group (FIG) a working group has started in 2021 to
create a ClockInterface. The works on that have been rather fast and already in the mid of 
2021 the interface was more or less finally decided upon. 

### So why this Interface?

Since mid 2021 no further work has been happening on the Working Group. All requests towards
the editor and the sponsor weren't met with any reaction.

So after a lot of discussions on the official working group channel I decided to bring this 
interface forward by providing the currently agreed upon interface as a separate package 
on packagist.

### But what when the PSR Interface is provided?

There are two possibilities: 
* Either the interface will be provided by the FIG as it is currently,
then this interface will extend the PSR-20 one so that all implementations of this 
interface will be immediately PSR20 compatible. 
* Or the PSR20 interface will look different: Then all current implementations will
need to provide a spearate implementation for PSR20 compatibility and this interface will 
simply coexist with the PSR20 one.

## Documentation

For a more thorough information about the interface please check the PSR-20 documentation 
at https://github.com/php-fig/fig-standards/blob/master/proposed/clock.md and 
https://github.com/php-fig/fig-standards/blob/master/proposed/clock-meta.md

