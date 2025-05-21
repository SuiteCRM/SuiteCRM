<?php
/**
 * This file is part of the Composer Merge plugin.
 *
 * Copyright (C) 2015 Bryan Davis, Wikimedia Foundation, and contributors
 *
 * This software may be modified and distributed under the terms of the MIT
 * license. See the LICENSE file for details.
 */

namespace Wikimedia\Composer\Merge\V2;

use Composer\Composer;
use Composer\Json\JsonFile;
use Composer\Package\BasePackage;
use Composer\Package\CompletePackage;
use Composer\Package\Link;
use Composer\Package\Loader\ArrayLoader;
use Composer\Package\RootAliasPackage;
use Composer\Package\RootPackage;
use Composer\Package\RootPackageInterface;
use Composer\Package\Version\VersionParser;
use Composer\Semver\Intervals;
use UnexpectedValueException;

/**
 * Processing for a composer.json file that will be merged into
 * a RootPackageInterface
 *
 * @author Bryan Davis <bd808@bd808.com>
 */
class ExtraPackage
{

    /**
     * @var Composer $composer
     */
    protected $composer;

    /**
     * @var Logger $logger
     */
    protected $logger;

    /**
     * @var string $path
     */
    protected $path;

    /**
     * @var array $json
     */
    protected $json;

    /**
     * @var CompletePackage $package
     */
    protected $package;

    /**
     * @var array<string, bool> $mergedRequirements
     */
    protected $mergedRequirements = [];

    /**
     * @var VersionParser $versionParser
     */
    protected $versionParser;

    /**
     * @param string $path Path to composer.json file
     * @param Composer $composer
     * @param Logger $logger
     */
    public function __construct($path, Composer $composer, Logger $logger)
    {
        $this->path = $path;
        $this->composer = $composer;
        $this->logger = $logger;
        $this->json = $this->readPackageJson($path);
        $this->package = $this->loadPackage($this->json);
        $this->versionParser = new VersionParser();
    }

    /**
     * Get list of additional packages to include if precessing recursively.
     *
     * @return array
     */
    public function getIncludes()
    {
        return isset($this->json['extra']['merge-plugin']['include']) ?
            $this->fixRelativePaths($this->json['extra']['merge-plugin']['include']) : [];
    }

    /**
     * Get list of additional packages to require if precessing recursively.
     *
     * @return array
     */
    public function getRequires()
    {
        return isset($this->json['extra']['merge-plugin']['require']) ?
            $this->fixRelativePaths($this->json['extra']['merge-plugin']['require']) : [];
    }

    /**
     * Get list of merged requirements from this package.
     *
     * @return string[]
     */
    public function getMergedRequirements()
    {
        return array_keys($this->mergedRequirements);
    }

    /**
     * Read the contents of a composer.json style file into an array.
     *
     * The package contents are fixed up to be usable to create a Package
     * object by providing dummy "name" and "version" values if they have not
     * been provided in the file. This is consistent with the default root
     * package loading behavior of Composer.
     *
     * @param string $path
     * @return array
     */
    protected function readPackageJson($path)
    {
        $file = new JsonFile($path);
        $json = $file->read();
        if (!isset($json['name'])) {
            $json['name'] = 'merge-plugin/' .
                strtr($path, DIRECTORY_SEPARATOR, '-');
        }
        if (!isset($json['version'])) {
            $json['version'] = '1.0.0';
        }
        return $json;
    }

    /**
     * @param array $json
     * @return CompletePackage
     */
    protected function loadPackage(array $json)
    {
        $loader = new ArrayLoader();
        $package = $loader->load($json);
        // @codeCoverageIgnoreStart
        if (!$package instanceof CompletePackage) {
            throw new UnexpectedValueException(
                'Expected instance of CompletePackage, got ' .
                get_class($package)
            );
        }
        // @codeCoverageIgnoreEnd
        return $package;
    }

    /**
     * Merge this package into a RootPackageInterface
     *
     * @param RootPackageInterface $root
     * @param PluginState $state
     */
    public function mergeInto(RootPackageInterface $root, PluginState $state)
    {
        $this->prependRepositories($root);

        $this->mergeRequires('require', $root, $state);

        $this->mergePackageLinks('conflict', $root);

        if ($state->shouldMergeReplace()) {
            $this->mergePackageLinks('replace', $root);
        }

        $this->mergePackageLinks('provide', $root);

        $this->mergeSuggests($root);

        $this->mergeAutoload('autoload', $root);

        $this->mergeExtra($root, $state);

        $this->mergeScripts($root, $state);

        if ($state->isDevMode()) {
            $this->mergeDevInto($root, $state);
        } else {
            $this->mergeReferences($root);
            $this->mergeAliases($root);
        }
    }

    /**
     * Merge just the dev portion into a RootPackageInterface
     *
     * @param RootPackageInterface $root
     * @param PluginState $state
     */
    public function mergeDevInto(RootPackageInterface $root, PluginState $state)
    {
        $this->mergeRequires('require-dev', $root, $state);
        $this->mergeAutoload('devAutoload', $root);
        $this->mergeReferences($root);
        $this->mergeAliases($root);
    }

    /**
     * Add a collection of repositories described by the given configuration
     * to the given package and the global repository manager.
     *
     * @param RootPackageInterface $root
     */
    protected function prependRepositories(RootPackageInterface $root)
    {
        if (!isset($this->json['repositories'])) {
            return;
        }
        $repoManager = $this->composer->getRepositoryManager();
        $newRepos = [];

        foreach ($this->json['repositories'] as $repoJson) {
            if (!isset($repoJson['type'])) {
                continue;
            }
            if ($repoJson['type'] === 'path' && isset($repoJson['url'])) {
                $repoJson['url'] = $this->fixRelativePaths(array($repoJson['url']))[0];
            }
            $this->logger->info("Prepending {$repoJson['type']} repository");
            $repo = $repoManager->createRepository(
                $repoJson['type'],
                $repoJson
            );
            $repoManager->prependRepository($repo);
            $newRepos[] = $repo;
        }

        $unwrapped = self::unwrapIfNeeded($root, 'setRepositories');
        $unwrapped->setRepositories(array_merge(
            $newRepos,
            $root->getRepositories()
        ));
    }

    /**
     * Merge require or require-dev into a RootPackageInterface
     *
     * @param string $type 'require' or 'require-dev'
     * @param RootPackageInterface $root
     * @param PluginState $state
     */
    protected function mergeRequires(
        $type,
        RootPackageInterface $root,
        PluginState $state
    ) {
        $linkType = BasePackage::$supportedLinkTypes[$type];
        $getter = 'get' . ucfirst($linkType['method']);
        $setter = 'set' . ucfirst($linkType['method']);

        $requires = $this->package->{$getter}();
        if (empty($requires)) {
            return;
        }

        $this->mergeStabilityFlags($root, $requires);

        $requires = $this->replaceSelfVersionDependencies(
            $type,
            $requires,
            $root
        );

        $root->{$setter}($this->mergeOrDefer(
            $type,
            $root->{$getter}(),
            $requires,
            $state
        ));
    }

    /**
     * Merge two collections of package links and collect duplicates for
     * subsequent processing.
     *
     * @param string $type 'require' or 'require-dev'
     * @param array $origin Primary collection
     * @param array $merge Additional collection
     * @param PluginState $state
     * @return array Merged collection
     */
    protected function mergeOrDefer(
        $type,
        array $origin,
        array $merge,
        PluginState $state
    ) {
        if ($state->ignoreDuplicateLinks() && $state->replaceDuplicateLinks()) {
            $this->logger->warning("Both replace and ignore-duplicates are true. These are mutually exclusive.");
            $this->logger->warning("Duplicate packages will be ignored.");
        }

        foreach ($merge as $name => $link) {
            if (isset($origin[$name])) {
                if ($state->ignoreDuplicateLinks()) {
                    $this->logger->info("Ignoring duplicate <comment>{$name}</comment>");
                    continue;
                }

                if ($state->replaceDuplicateLinks()) {
                    $this->logger->info("Replacing <comment>{$name}</comment>");
                    $this->mergedRequirements[$name] = true;
                    $origin[$name] = $link;
                } else {
                    $this->logger->info("Merging <comment>{$name}</comment>");
                    $this->mergedRequirements[$name] = true;
                    $origin[$name] = $this->mergeConstraints($origin[$name], $link, $state);
                }
            } else {
                $this->logger->info("Adding <comment>{$name}</comment>");
                $this->mergedRequirements[$name] = true;
                $origin[$name] = $link;
            }
        }

        if (!$state->isComposer1()) {
            Intervals::clear();
        }

        return $origin;
    }

    /**
     * Merge package constraints.
     *
     * Adapted from Composer's UpdateCommand::appendConstraintToLink
     *
     * @param Link $origin The base package link.
     * @param Link $merge The related package link to merge.
     * @param PluginState $state
     * @return Link Merged link.
     */
    protected function mergeConstraints(Link $origin, Link $merge, PluginState $state)
    {
        $oldPrettyString = $origin->getConstraint()->getPrettyString();
        $newPrettyString = $merge->getConstraint()->getPrettyString();

        if ($state->isComposer1()) {
            $constraintClass = MultiConstraint::class;
        } else {
            $constraintClass = \Composer\Semver\Constraint\MultiConstraint::class;

            if (Intervals::isSubsetOf($origin->getConstraint(), $merge->getConstraint())) {
                return $origin;
            }

            if (Intervals::isSubsetOf($merge->getConstraint(), $origin->getConstraint())) {
                return $merge;
            }
        }

        $newConstraint = $constraintClass::create([
            $origin->getConstraint(),
            $merge->getConstraint()
        ], true);
        $newConstraint->setPrettyString($oldPrettyString.', '.$newPrettyString);

        return new Link(
            $origin->getSource(),
            $origin->getTarget(),
            $newConstraint,
            $origin->getDescription(),
            $origin->getPrettyConstraint() . ', ' . $newPrettyString
        );
    }

    /**
     * Merge autoload or autoload-dev into a RootPackageInterface
     *
     * @param string $type 'autoload' or 'devAutoload'
     * @param RootPackageInterface $root
     */
    protected function mergeAutoload($type, RootPackageInterface $root)
    {
        $getter = 'get' . ucfirst($type);
        $setter = 'set' . ucfirst($type);

        $autoload = $this->package->{$getter}();
        if (empty($autoload)) {
            return;
        }

        $unwrapped = self::unwrapIfNeeded($root, $setter);
        $unwrapped->{$setter}(array_merge_recursive(
            $root->{$getter}(),
            $this->fixRelativePaths($autoload)
        ));
    }

    /**
     * Fix a collection of paths that are relative to this package to be
     * relative to the base package.
     *
     * @param array $paths
     * @return array
     */
    protected function fixRelativePaths(array $paths)
    {
        $base = dirname($this->path);
        $base = ($base === '.') ? '' : "{$base}/";

        array_walk_recursive(
            $paths,
            function (&$path) use ($base) {
                $path = "{$base}{$path}";
            }
        );
        return $paths;
    }

    /**
     * Extract and merge stability flags from the given collection of
     * requires and merge them into a RootPackageInterface
     *
     * @param RootPackageInterface $root
     * @param array $requires
     */
    protected function mergeStabilityFlags(
        RootPackageInterface $root,
        array $requires
    ) {
        $flags = $root->getStabilityFlags();
        $sf = new StabilityFlags($flags, $root->getMinimumStability());

        $unwrapped = self::unwrapIfNeeded($root, 'setStabilityFlags');
        $unwrapped->setStabilityFlags(array_merge(
            $flags,
            $sf->extractAll($requires)
        ));
    }

    /**
     * Merge package links of the given type into a RootPackageInterface
     *
     * @param string $type 'conflict', 'replace' or 'provide'
     * @param RootPackageInterface $root
     */
    protected function mergePackageLinks($type, RootPackageInterface $root)
    {
        $linkType = BasePackage::$supportedLinkTypes[$type];
        $getter = 'get' . ucfirst($linkType['method']);
        $setter = 'set' . ucfirst($linkType['method']);

        $links = $this->package->{$getter}();
        if (!empty($links)) {
            $unwrapped = self::unwrapIfNeeded($root, $setter);
            // @codeCoverageIgnoreStart
            if ($root !== $unwrapped) {
                $this->logger->warning(
                    'This Composer version does not support ' .
                    "'{$type}' merging for aliased packages."
                );
            }
            // @codeCoverageIgnoreEnd
            $unwrapped->{$setter}(array_merge(
                $root->{$getter}(),
                $this->replaceSelfVersionDependencies($type, $links, $root)
            ));
        }
    }

    /**
     * Merge suggested packages into a RootPackageInterface
     *
     * @param RootPackageInterface $root
     */
    protected function mergeSuggests(RootPackageInterface $root)
    {
        $suggests = $this->package->getSuggests();
        if (!empty($suggests)) {
            $unwrapped = self::unwrapIfNeeded($root, 'setSuggests');
            $unwrapped->setSuggests(array_merge(
                $root->getSuggests(),
                $suggests
            ));
        }
    }

    /**
     * Merge extra config into a RootPackageInterface
     *
     * @param RootPackageInterface $root
     * @param PluginState $state
     */
    public function mergeExtra(RootPackageInterface $root, PluginState $state)
    {
        $extra = $this->package->getExtra();
        unset($extra['merge-plugin']);
        if (!$state->shouldMergeExtra() || empty($extra)) {
            return;
        }

        $rootExtra = $root->getExtra();
        $unwrapped = self::unwrapIfNeeded($root, 'setExtra');

        if ($state->replaceDuplicateLinks()) {
            $unwrapped->setExtra(
                self::mergeExtraArray($state->shouldMergeExtraDeep(), $rootExtra, $extra)
            );
        } else {
            if (!$state->shouldMergeExtraDeep()) {
                foreach (array_intersect(
                    array_keys($extra),
                    array_keys($rootExtra)
                ) as $key) {
                    $this->logger->info(
                        "Ignoring duplicate <comment>{$key}</comment> in ".
                        "<comment>{$this->path}</comment> extra config."
                    );
                }
            }
            $unwrapped->setExtra(
                self::mergeExtraArray($state->shouldMergeExtraDeep(), $extra, $rootExtra)
            );
        }
    }

    /**
     * Merge scripts config into a RootPackageInterface
     *
     * @param RootPackageInterface $root
     * @param PluginState $state
     */
    public function mergeScripts(RootPackageInterface $root, PluginState $state)
    {
        $scripts = $this->package->getScripts();
        if (!$state->shouldMergeScripts() || empty($scripts)) {
            return;
        }

        $rootScripts = $root->getScripts();
        $unwrapped = self::unwrapIfNeeded($root, 'setScripts');

        if ($state->replaceDuplicateLinks()) {
            $unwrapped->setScripts(
                array_merge($rootScripts, $scripts)
            );
        } else {
            $unwrapped->setScripts(
                array_merge($scripts, $rootScripts)
            );
        }
    }

    /**
     * Merges two arrays either via arrayMergeDeep or via array_merge.
     *
     * @param bool $mergeDeep
     * @param array $array1
     * @param array $array2
     * @return array
     */
    public static function mergeExtraArray($mergeDeep, $array1, $array2)
    {
        if ($mergeDeep) {
            return NestedArray::mergeDeep($array1, $array2);
        }

        return array_merge($array1, $array2);
    }

    /**
     * Update Links with a 'self.version' constraint with the root package's
     * version.
     *
     * @param string $type Link type
     * @param array $links
     * @param RootPackageInterface $root
     * @return array
     */
    protected function replaceSelfVersionDependencies(
        $type,
        array $links,
        RootPackageInterface $root
    ) {
        $linkType = BasePackage::$supportedLinkTypes[$type];
        $version = $root->getVersion();
        $prettyVersion = $root->getPrettyVersion();
        $vp = $this->versionParser;

        $method = 'get' . ucfirst($linkType['method']);
        $packages = $root->$method();

        return array_map(
            static function ($link) use ($linkType, $version, $prettyVersion, $vp, $packages) {
                if ('self.version' === $link->getPrettyConstraint()) {
                    if (isset($packages[$link->getSource()])) {
                        /** @var Link $package */
                        $package = $packages[$link->getSource()];
                        return new Link(
                            $link->getSource(),
                            $link->getTarget(),
                            $vp->parseConstraints($package->getConstraint()->getPrettyString()),
                            $linkType['description'],
                            $package->getPrettyConstraint()
                        );
                    }

                    return new Link(
                        $link->getSource(),
                        $link->getTarget(),
                        $vp->parseConstraints($version),
                        $linkType['description'],
                        $prettyVersion
                    );
                }
                return $link;
            },
            $links
        );
    }

    /**
     * Get a full featured Package from a RootPackageInterface.
     *
     * In Composer versions before 599ad77 the RootPackageInterface only
     * defines a sub-set of operations needed by composer-merge-plugin and
     * RootAliasPackage only implemented those methods defined by the
     * interface. Most of the unimplemented methods in RootAliasPackage can be
     * worked around because the getter methods that are implemented proxy to
     * the aliased package which we can modify by unwrapping. The exception
     * being modifying the 'conflicts', 'provides' and 'replaces' collections.
     * We have no way to actually modify those collections unfortunately in
     * older versions of Composer.
     *
     * @param RootPackageInterface $root
     * @param string $method Method needed
     * @return RootPackageInterface|RootPackage
     */
    public static function unwrapIfNeeded(
        RootPackageInterface $root,
        $method = 'setExtra'
    ) {
        // @codeCoverageIgnoreStart
        if ($root instanceof RootAliasPackage &&
            !method_exists($root, $method)
        ) {
            // Unwrap and return the aliased RootPackage.
            $root = $root->getAliasOf();
        }
        // @codeCoverageIgnoreEnd
        return $root;
    }

    protected function mergeAliases(RootPackageInterface $root)
    {
        $aliases = [];
        $unwrapped = self::unwrapIfNeeded($root, 'setAliases');
        foreach (array('require', 'require-dev') as $linkType) {
            $linkInfo = BasePackage::$supportedLinkTypes[$linkType];
            $method = 'get'.ucfirst($linkInfo['method']);
            $links = [];
            foreach ($unwrapped->$method() as $link) {
                $links[$link->getTarget()] = $link->getConstraint()->getPrettyString();
            }
            $aliases = $this->extractAliases($links, $aliases);
        }
        $unwrapped->setAliases($aliases);
    }

    /**
     * Extract aliases from version constraints (dev-branch as 1.0.0).
     *
     * @param array $requires
     * @param array $aliases
     * @return array
     * @see RootPackageLoader::extractAliases()
     */
    protected function extractAliases(array $requires, array $aliases)
    {
        foreach ($requires as $reqName => $reqVersion) {
            if (preg_match('{^([^,\s#]+)(?:#[^ ]+)? +as +([^,\s]+)$}', $reqVersion, $match)) {
                $aliases[] = [
                    'package' => strtolower($reqName),
                    'version' => $this->versionParser->normalize($match[1], $reqVersion),
                    'alias' => $match[2],
                    'alias_normalized' => $this->versionParser->normalize($match[2], $reqVersion),
                ];
            } elseif (strpos($reqVersion, ' as ') !== false) {
                throw new UnexpectedValueException(
                    'Invalid alias definition in "'.$reqName.'": "'.$reqVersion.'". '
                    . 'Aliases should be in the form "exact-version as other-exact-version".'
                );
            }
        }

        return $aliases;
    }

    /**
     * Update the root packages reference information.
     *
     * @param RootPackageInterface $root
     */
    protected function mergeReferences(RootPackageInterface $root)
    {
        // Merge source reference information for merged packages.
        // @see RootPackageLoader::load
        $references = [];
        $unwrapped = self::unwrapIfNeeded($root, 'setReferences');
        foreach (['require', 'require-dev'] as $linkType) {
            $linkInfo = BasePackage::$supportedLinkTypes[$linkType];
            $method = 'get'.ucfirst($linkInfo['method']);
            $links = [];
            foreach ($unwrapped->$method() as $link) {
                $links[$link->getTarget()] = $link->getConstraint()->getPrettyString();
            }
            $references = $this->extractReferences($links, $references);
        }
        $unwrapped->setReferences($references);
    }

    /**
     * Extract vcs revision from version constraint (dev-master#abc123.
     *
     * @param array $requires
     * @param array $references
     * @return array
     * @see RootPackageLoader::extractReferences()
     */
    protected function extractReferences(array $requires, array $references)
    {
        foreach ($requires as $reqName => $reqVersion) {
            $reqVersion = preg_replace('{^([^,\s@]+) as .+$}', '$1', $reqVersion);
            $stabilityName = VersionParser::parseStability($reqVersion);
            if ($stabilityName === 'dev' &&
                preg_match('{^[^,\s@]+?#([a-f0-9]+)$}', $reqVersion, $match)
            ) {
                $name = strtolower($reqName);
                $references[$name] = $match[1];
            }
        }

        return $references;
    }
}
// vim:sw=4:ts=4:sts=4:et:
