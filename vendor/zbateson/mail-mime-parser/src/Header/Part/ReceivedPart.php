<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Part;

use ZBateson\MbWrapper\MbWrapper;

/**
 * Represents one parameter in a parsed 'Received' header, e.g. the FROM or VIA
 * part.
 *
 * Note that FROM and BY actually get parsed into a sub-class,
 * ReceivedDomainPart which keeps track of other sub-parts that can be parsed
 * from them.
 *
 * @author Zaahid Bateson
 */
class ReceivedPart extends ParameterPart
{
    /**
     * Constructor.
     *
     * @param string $name
     * @param string $value
     */
    public function __construct(MbWrapper $charsetConverter, $name, $value) {
        parent::__construct($charsetConverter, '', '');
        // can't be mime-encoded
        $this->name = \trim($name);
        $this->value = $value ? \trim($value) : $value;
    }
}
