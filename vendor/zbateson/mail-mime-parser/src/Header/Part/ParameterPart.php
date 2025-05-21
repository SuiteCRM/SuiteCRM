<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Part;

use ZBateson\MbWrapper\MbWrapper;

/**
 * Represents a name/value pair part of a header.
 *
 * @author Zaahid Bateson
 */
class ParameterPart extends MimeLiteralPart
{
    /**
     * @var string the name of the parameter
     */
    protected $name;

    /**
     * @var string the RFC-1766 language tag if set.
     */
    protected $language;

    /**
     * Constructs a ParameterPart out of a name/value pair.  The name and
     * value are both mime-decoded if necessary.
     *
     * If $language is provided, $name and $value are not mime-decoded. Instead,
     * they're taken as literals as part of a SplitParameterToken.
     *
     * @param string $name
     * @param string $value
     * @param string $language
     */
    public function __construct(MbWrapper $charsetConverter, $name, $value, $language = null)
    {
        if ($language !== null) {
            parent::__construct($charsetConverter, '');
            $this->name = $name;
            $this->value = $value;
            $this->language = $language;
        } else {
            parent::__construct($charsetConverter, \trim($value));
            $this->name = $this->decodeMime(\trim($name));
        }
    }

    /**
     * Returns the name of the parameter.
     *
     * @return string The name.
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * Returns the RFC-1766 (or subset) language tag, if the parameter is a
     * split RFC-2231 part with a language tag set.
     *
     * @return string the language
     */
    public function getLanguage()
    {
        return $this->language;
    }
}
