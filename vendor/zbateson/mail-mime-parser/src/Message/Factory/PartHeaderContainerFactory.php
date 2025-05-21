<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message\Factory;

use ZBateson\MailMimeParser\Header\HeaderFactory;
use ZBateson\MailMimeParser\Message\PartHeaderContainer;

/**
 * Creates PartHeaderContainer instances.
 *
 * @author Zaahid Bateson
 */
class PartHeaderContainerFactory
{
    /**
     * @var HeaderFactory the HeaderFactory passed to HeaderContainer instances.
     */
    protected $headerFactory;

    /**
     * Constructor
     *
     */
    public function __construct(HeaderFactory $headerFactory)
    {
        $this->headerFactory = $headerFactory;
    }

    /**
     * Creates and returns a PartHeaderContainer.
     *
     * @return PartHeaderContainer
     */
    public function newInstance(?PartHeaderContainer $from = null)
    {
        return new PartHeaderContainer($this->headerFactory, $from);
    }
}
