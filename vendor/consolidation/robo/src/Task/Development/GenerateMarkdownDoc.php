<?php

namespace Robo\Task\Development;

use Robo\Task\BaseTask;
use Robo\Result;
use Robo\Contract\BuilderAwareInterface;
use Robo\Common\BuilderAwareTrait;

/**
 * Simple documentation generator from source files.
 * Takes classes, properties and methods with their docblocks and writes down a markdown file.
 *
 * ``` php
 * <?php
 * $this->taskGenDoc('models.md')
 *      ->docClass('Model\User') // take class Model\User
 *      ->docClass('Model\Post') // take class Model\Post
 *      ->filterMethods(function(\ReflectionMethod $r) {
 *          return $r->isPublic() or $r->isProtected(); // process public and protected methods
 *      })->processClass(function(\ReflectionClass $r, $text) {
 *          return "Class ".$r->getName()."\n\n$text\n\n###Methods\n";
 *      })->run();
 * ```
 *
 * By default this task generates a documentation for each public method of a class, interface or trait.
 * It combines method signature with a docblock. Both can be post-processed.
 *
 * ``` php
 * <?php
 * $this->taskGenDoc('models.md')
 *      ->docClass('Model\User')
 *      ->processClassSignature(false) // false can be passed to not include class signature
 *      ->processClassDocBlock(function(\ReflectionClass $r, $text) {
 *          return "[This is part of application model]\n" . $text;
 *      })->processMethodSignature(function(\ReflectionMethod $r, $text) {
 *          return "#### {$r->name}()";
 *      })->processMethodDocBlock(function(\ReflectionMethod $r, $text) {
 *          return strpos($r->name, 'save')===0 ? "[Saves to the database]\n" . $text : $text;
 *      })->run();
 * ```
 */
class GenerateMarkdownDoc extends BaseTask implements BuilderAwareInterface
{
    use BuilderAwareTrait;

    /**
     * @var string[]
     */
    protected $docClass = [];

    /**
     * @var callable
     */
    protected $filterMethods;

    /**
     * @var callable
     */
    protected $filterClasses;

    /**
     * @var callable
     */
    protected $filterProperties;

    /**
     * @var callable
     */
    protected $processClass;

    /**
     * @var callable|false
     */
    protected $processClassSignature;

    /**
     * @var callable|false
     */
    protected $processClassDocBlock;

    /**
     * @var callable|false
     */
    protected $processMethod;

    /**
     * @var callable|false
     */
    protected $processMethodSignature;

    /**
     * @var callable|false
     */
    protected $processMethodDocBlock;

    /**
     * @var callable|false
     */
    protected $processProperty;

    /**
     * @var callable|false
     */
    protected $processPropertySignature;

    /**
     * @var callable|false
     */
    protected $processPropertyDocBlock;

    /**
     * @var callable
     */
    protected $reorder;

    /**
     * @var callable
     */
    protected $reorderMethods;

    /**
     * @todo Unused property.
     *
     * @var callable
     */
    protected $reorderProperties;

    /**
     * @var string
     */
    protected $filename;

    /**
     * @var string
     */
    protected $prepend = "";

    /**
     * @var string
     */
    protected $append = "";

    /**
     * @var string
     */
    protected $text;

    /**
     * @var string[]
     */
    protected $textForClass = [];

    /**
     * @param string $filename
     *
     * @return static
     */
    public static function init($filename)
    {
        return new static($filename);
    }

    /**
     * @param string $filename
     */
    public function __construct($filename)
    {
        $this->filename = $filename;
    }

    /**
     * Put a class you want to be documented.
     *
     * @param string $item
     *
     * @return $this
     */
    public function docClass($item)
    {
        $this->docClass[] = $item;
        return $this;
    }

    /**
     * Using a callback function filter out methods that won't be documented.
     *
     * @param callable $filterMethods
     *
     * @return $this
     */
    public function filterMethods($filterMethods)
    {
        $this->filterMethods = $filterMethods;
        return $this;
    }

    /**
     * Using a callback function filter out classes that won't be documented.
     *
     * @param callable $filterClasses
     *
     * @return $this
     */
    public function filterClasses($filterClasses)
    {
        $this->filterClasses = $filterClasses;
        return $this;
    }

    /**
     * Using a callback function filter out properties that won't be documented.
     *
     * @param callable $filterProperties
     *
     * @return $this
     */
    public function filterProperties($filterProperties)
    {
        $this->filterProperties = $filterProperties;
        return $this;
    }

    /**
     * Post-process class documentation.
     *
     * @param callable $processClass
     *
     * @return $this
     */
    public function processClass($processClass)
    {
        $this->processClass = $processClass;
        return $this;
    }

    /**
     * Post-process class signature. Provide *false* to skip.
     *
     * @param callable|false $processClassSignature
     *
     * @return $this
     */
    public function processClassSignature($processClassSignature)
    {
        $this->processClassSignature = $processClassSignature;
        return $this;
    }

    /**
     * Post-process class docblock contents. Provide *false* to skip.
     *
     * @param callable|false $processClassDocBlock
     *
     * @return $this
     */
    public function processClassDocBlock($processClassDocBlock)
    {
        $this->processClassDocBlock = $processClassDocBlock;
        return $this;
    }

    /**
     * Post-process method documentation. Provide *false* to skip.
     *
     * @param callable|false $processMethod
     *
     * @return $this
     */
    public function processMethod($processMethod)
    {
        $this->processMethod = $processMethod;
        return $this;
    }

    /**
     * Post-process method signature. Provide *false* to skip.
     *
     * @param callable|false $processMethodSignature
     *
     * @return $this
     */
    public function processMethodSignature($processMethodSignature)
    {
        $this->processMethodSignature = $processMethodSignature;
        return $this;
    }

    /**
     * Post-process method docblock contents. Provide *false* to skip.
     *
     * @param callable|false $processMethodDocBlock
     *
     * @return $this
     */
    public function processMethodDocBlock($processMethodDocBlock)
    {
        $this->processMethodDocBlock = $processMethodDocBlock;
        return $this;
    }

    /**
     * Post-process property documentation. Provide *false* to skip.
     *
     * @param callable|false $processProperty
     *
     * @return $this
     */
    public function processProperty($processProperty)
    {
        $this->processProperty = $processProperty;
        return $this;
    }

    /**
     * Post-process property signature. Provide *false* to skip.
     *
     * @param callable|false $processPropertySignature
     *
     * @return $this
     */
    public function processPropertySignature($processPropertySignature)
    {
        $this->processPropertySignature = $processPropertySignature;
        return $this;
    }

    /**
     * Post-process property docblock contents. Provide *false* to skip.
     *
     * @param callable|false $processPropertyDocBlock
     *
     * @return $this
     */
    public function processPropertyDocBlock($processPropertyDocBlock)
    {
        $this->processPropertyDocBlock = $processPropertyDocBlock;
        return $this;
    }

    /**
     * Use a function to reorder classes.
     *
     * @param callable $reorder
     *
     * @return $this
     */
    public function reorder($reorder)
    {
        $this->reorder = $reorder;
        return $this;
    }

    /**
     * Use a function to reorder methods in class.
     *
     * @param callable $reorderMethods
     *
     * @return $this
     */
    public function reorderMethods($reorderMethods)
    {
        $this->reorderMethods = $reorderMethods;
        return $this;
    }

    /**
     * @param callable $reorderProperties
     *
     * @return $this
     */
    public function reorderProperties($reorderProperties)
    {
        $this->reorderProperties = $reorderProperties;
        return $this;
    }

    /**
     * @param string $filename
     *
     * @return $this
     */
    public function filename($filename)
    {
        $this->filename = $filename;
        return $this;
    }

    /**
     * Inserts text at the beginning of markdown file.
     *
     * @param string $prepend
     *
     * @return $this
     */
    public function prepend($prepend)
    {
        $this->prepend = $prepend;
        return $this;
    }

    /**
     * Inserts text at the end of markdown file.
     *
     * @param string $append
     *
     * @return $this
     */
    public function append($append)
    {
        $this->append = $append;
        return $this;
    }

    /**
     * @param string $text
     *
     * @return $this
     */
    public function text($text)
    {
        $this->text = $text;
        return $this;
    }

    /**
     * @param string $item
     *
     * @return $this
     */
    public function textForClass($item)
    {
        $this->textForClass[] = $item;
        return $this;
    }

    /**
     * {@inheritdoc}
     */
    public function run()
    {
        foreach ($this->docClass as $class) {
            $this->printTaskInfo("Processing {class}", ['class' => $class]);
            $this->textForClass[$class] = $this->documentClass($class);
        }

        if (is_callable($this->reorder)) {
            $this->printTaskInfo("Applying reorder function");
            call_user_func_array($this->reorder, [$this->textForClass]);
        }

        $this->text = implode("\n", $this->textForClass);

        /** @var \Robo\Result $result */
        $result = $this->collectionBuilder()->taskWriteToFile($this->filename)
            ->line($this->prepend)
            ->text($this->text)
            ->line($this->append)
            ->run();

        $this->printTaskSuccess('{filename} created. {class-count} classes documented', ['filename' => $this->filename, 'class-count' => count($this->docClass)]);

        return new Result($this, $result->getExitCode(), $result->getMessage(), $this->textForClass);
    }

    /**
     * @param string $class
     *
     * @return null|string
     */
    protected function documentClass($class)
    {
        if (!class_exists($class) && !trait_exists($class)) {
            return "";
        }
        $refl = new \ReflectionClass($class);

        if (is_callable($this->filterClasses)) {
            $ret = call_user_func($this->filterClasses, $refl);
            if (!$ret) {
                return;
            }
        }
        $doc = $this->documentClassSignature($refl);
        $doc .= "\n" . $this->documentClassDocBlock($refl);
        $doc .= "\n";

        if (is_callable($this->processClass)) {
            $doc = call_user_func($this->processClass, $refl, $doc);
        }

        $properties = [];
        foreach ($refl->getProperties() as $reflProperty) {
            $properties[] = $this->documentProperty($reflProperty);
        }

        $properties = array_filter($properties);
        $doc .= implode("\n", $properties);

        $methods = [];
        foreach ($refl->getMethods() as $reflMethod) {
            $methods[$reflMethod->name] = $this->documentMethod($reflMethod);
        }
        if (is_callable($this->reorderMethods)) {
            call_user_func_array($this->reorderMethods, [&$methods]);
        }

        $methods = array_filter($methods);

        $doc .= implode("\n", $methods) . "\n";

        return $doc;
    }

    /**
     * @param \ReflectionClass $reflectionClass
     *
     * @return string
     */
    protected function documentClassSignature(\ReflectionClass $reflectionClass)
    {
        if ($this->processClassSignature === false) {
            return "";
        }

        $signature = "## {$reflectionClass->name}\n\n";

        if ($parent = $reflectionClass->getParentClass()) {
            $signature .= "* *Extends* `{$parent->name}`";
        }
        $interfaces = $reflectionClass->getInterfaceNames();
        if (count($interfaces)) {
            $signature .= "\n* *Implements* `" . implode('`, `', $interfaces) . '`';
        }
        $traits = $reflectionClass->getTraitNames();
        if (count($traits)) {
            $signature .= "\n* *Uses* `" . implode('`, `', $traits) . '`';
        }
        if (is_callable($this->processClassSignature)) {
            $signature = call_user_func($this->processClassSignature, $reflectionClass, $signature);
        }

        return $signature;
    }

    /**
     * @param \ReflectionClass $reflectionClass
     *
     * @return string
     */
    protected function documentClassDocBlock(\ReflectionClass $reflectionClass)
    {
        if ($this->processClassDocBlock === false) {
            return "";
        }
        $doc = self::indentDoc($reflectionClass->getDocComment());
        if (is_callable($this->processClassDocBlock)) {
            $doc = call_user_func($this->processClassDocBlock, $reflectionClass, $doc);
        }
        return $doc;
    }

    /**
     * @param \ReflectionMethod $reflectedMethod
     *
     * @return string
     */
    protected function documentMethod(\ReflectionMethod $reflectedMethod)
    {
        if ($this->processMethod === false) {
            return "";
        }
        if (is_callable($this->filterMethods)) {
            $ret = call_user_func($this->filterMethods, $reflectedMethod);
            if (!$ret) {
                return "";
            }
        } else {
            if (!$reflectedMethod->isPublic()) {
                return "";
            }
        }

        $signature = $this->documentMethodSignature($reflectedMethod);
        $docblock = $this->documentMethodDocBlock($reflectedMethod);
        $methodDoc = "$signature $docblock";
        if (is_callable($this->processMethod)) {
            $methodDoc = call_user_func($this->processMethod, $reflectedMethod, $methodDoc);
        }
        return $methodDoc;
    }

    /**
     * @param \ReflectionProperty $reflectedProperty
     *
     * @return string
     */
    protected function documentProperty(\ReflectionProperty $reflectedProperty)
    {
        if ($this->processProperty === false) {
            return "";
        }
        if (is_callable($this->filterProperties)) {
            $ret = call_user_func($this->filterProperties, $reflectedProperty);
            if (!$ret) {
                return "";
            }
        } else {
            if (!$reflectedProperty->isPublic()) {
                return "";
            }
        }
        $signature = $this->documentPropertySignature($reflectedProperty);
        $docblock = $this->documentPropertyDocBlock($reflectedProperty);
        $propertyDoc = $signature . $docblock;
        if (is_callable($this->processProperty)) {
            $propertyDoc = call_user_func($this->processProperty, $reflectedProperty, $propertyDoc);
        }
        return $propertyDoc;
    }

    /**
     * @param \ReflectionProperty $reflectedProperty
     *
     * @return string
     */
    protected function documentPropertySignature(\ReflectionProperty $reflectedProperty)
    {
        if ($this->processPropertySignature === false) {
            return "";
        }
        $modifiers = implode(' ', \Reflection::getModifierNames($reflectedProperty->getModifiers()));
        $signature = "#### *$modifiers* {$reflectedProperty->name}";
        if (is_callable($this->processPropertySignature)) {
            $signature = call_user_func($this->processPropertySignature, $reflectedProperty, $signature);
        }
        return $signature;
    }

    /**
     * @param \ReflectionProperty $reflectedProperty
     *
     * @return string
     */
    protected function documentPropertyDocBlock(\ReflectionProperty $reflectedProperty)
    {
        if ($this->processPropertyDocBlock === false) {
            return "";
        }
        $propertyDoc = $reflectedProperty->getDocComment();
        // take from parent
        if (!$propertyDoc) {
            $parent = $reflectedProperty->getDeclaringClass();
            while ($parent = $parent->getParentClass()) {
                if ($parent->hasProperty($reflectedProperty->name)) {
                    $propertyDoc = $parent->getProperty($reflectedProperty->name)->getDocComment();
                }
            }
        }
        $propertyDoc = self::indentDoc($propertyDoc, 7);
        $propertyDoc = preg_replace("~^@(.*?)([$\s])~", ' * `$1` $2', $propertyDoc); // format annotations
        if (is_callable($this->processPropertyDocBlock)) {
            $propertyDoc = call_user_func($this->processPropertyDocBlock, $reflectedProperty, $propertyDoc);
        }
        return ltrim($propertyDoc);
    }

    /**
     * @param \ReflectionParameter $param
     *
     * @return string
     */
    protected function documentParam(\ReflectionParameter $param)
    {
        $text = "";
        $paramType = $param->getType();
        if ($paramType instanceof \ReflectionNamedType) {
            if ($paramType->getName() === 'array') {
                $text .= 'array ';
            }
            if (($paramType->getName() === 'callable')) {
                $text .= 'callable ';
            }
        }
        $text .= '$' . $param->name;
        if ($param->isDefaultValueAvailable()) {
            if ($param->allowsNull()) {
                $text .= ' = null';
            } else {
                $text .= ' = ' . str_replace("\n", ' ', var_export($param->getDefaultValue(), true));
            }
        }

        return $text;
    }

    /**
     * @param string $doc
     * @param int $indent
     *
     * @return string
     */
    public static function indentDoc($doc, $indent = 3)
    {
        if (!$doc) {
            return $doc;
        }
        return implode(
            "\n",
            array_map(
                function ($line) use ($indent) {
                    return substr($line, $indent);
                },
                explode("\n", $doc)
            )
        );
    }

    /**
     * @param \ReflectionMethod $reflectedMethod
     *
     * @return string
     */
    protected function documentMethodSignature(\ReflectionMethod $reflectedMethod)
    {
        if ($this->processMethodSignature === false) {
            return "";
        }
        $modifiers = implode(' ', \Reflection::getModifierNames($reflectedMethod->getModifiers()));
        $params = implode(
            ', ',
            array_map(
                function ($p) {
                    return $this->documentParam($p);
                },
                $reflectedMethod->getParameters()
            )
        );
        $signature = "#### *$modifiers* {$reflectedMethod->name}($params)";
        if (is_callable($this->processMethodSignature)) {
            $signature = call_user_func($this->processMethodSignature, $reflectedMethod, $signature);
        }
        return $signature;
    }

    /**
     * @param \ReflectionMethod $reflectedMethod
     *
     * @return string
     */
    protected function documentMethodDocBlock(\ReflectionMethod $reflectedMethod)
    {
        if ($this->processMethodDocBlock === false) {
            return "";
        }
        $methodDoc = $reflectedMethod->getDocComment();
        // take from parent
        if (!$methodDoc) {
            $parent = $reflectedMethod->getDeclaringClass();
            while ($parent = $parent->getParentClass()) {
                if ($parent->hasMethod($reflectedMethod->name)) {
                    $methodDoc = $parent->getMethod($reflectedMethod->name)->getDocComment();
                }
            }
        }
        // take from interface
        if (!$methodDoc) {
            $interfaces = $reflectedMethod->getDeclaringClass()->getInterfaces();
            foreach ($interfaces as $interface) {
                $i = new \ReflectionClass($interface->name);
                if ($i->hasMethod($reflectedMethod->name)) {
                    $methodDoc = $i->getMethod($reflectedMethod->name)->getDocComment();
                    break;
                }
            }
        }

        $methodDoc = self::indentDoc($methodDoc, 7);
        $methodDoc = preg_replace("~^@(.*?) ([$\s])~m", ' * `$1` $2', $methodDoc); // format annotations
        if (is_callable($this->processMethodDocBlock)) {
            $methodDoc = call_user_func($this->processMethodDocBlock, $reflectedMethod, $methodDoc);
        }

        return $methodDoc;
    }
}
