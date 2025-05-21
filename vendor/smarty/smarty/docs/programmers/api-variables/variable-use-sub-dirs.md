\$use\_sub\_dirs {#variable.use.sub.dirs}
================

Smarty will create subdirectories under the [compiled
templates](#variable.compile.dir) and [cache](#variable.cache.dir)
directories if `$use_sub_dirs` is set to TRUE, default is FALSE. In an
environment where there are potentially tens of thousands of files
created, this may help the filesystem speed. On the other hand, some
environments do not allow PHP processes to create directories, so this
must be disabled which is the default.

Sub directories are more efficient, so use them if you can.
Theoretically you get much better performance on a filesystem with 10
directories each having 100 files, than with 1 directory having 1000
files. This was certainly the case with Solaris 7 (UFS)\... with newer
filesystems such as ext3 and especially reiserfs, the difference is
almost nothing.

> **Note**
>
> -   `$use_sub_dirs=true` doesn\'t work with
>     [safe\_mode=On](https://www.php.net/features.safe-mode), that\'s why
>     it\'s switchable and why it\'s off by default.
>
> -   `$use_sub_dirs=true` on Windows can cause problems.
>
> -   Safe\_mode is being deprecated in PHP6.
>
See also [`$compile_id`](#variable.compile.id),
[`$cache_dir`](#variable.cache.dir), and
[`$compile_dir`](#variable.compile.dir).
