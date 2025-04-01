<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */
namespace SuiteCRM\API\JsonApi\v1;

use Psr\Log\LoggerAwareInterface;
use Psr\Log\LoggerInterface;
use SuiteCRM\API\JsonApi\v1\Enumerator\LinksMessage;
use SuiteCRM\API\JsonApi\v1\Interfaces\JsonApiResponseInterface;
use SuiteCRM\Utility\SuiteLogger as Logger;

/**
 * Class Links
 * @package SuiteCRM\API\JsonApi\v1
 * @see http://jsonapi.org/format/1.0/#document-links
 */
#[\AllowDynamicProperties]
class Links implements LoggerAwareInterface, JsonApiResponseInterface
{
    /**
     * @var string $self
     */
    private $self;

    /**
     * @var string $first
     */
    private $first;

    /**
     * @var bool $hasPagination
     */
    private $hasPagination;

    /**
     * @var string $prev
     */
    private $prev;

    /**
     * @var string $next
     */
    private $next;

    /**
     * @var string $last
     */
    private $last;

    /**
     * @var string $href
     */
    private $href;

    /**
     * @var array $meta
     */
    private $meta;

    /**
     * @var string related
     */
    private $related;

    /**
     * @var LoggerInterface Logger
     */
    private $logger;

    /**
     * @return Links
     */
    public static function get()
    {
        return new self();
    }

    /**
     * @param string $url
     * @return Links
     */
    public function withSelf($url)
    {
        if ($this->validateUrl($url)) {
            $this->self = $url;
        } else {
            $this->getLogger()->error(LinksMessage::INVALID_URL_PARAMETER);
        }

        return clone $this;
    }

    /**
     * Tells Links that you intend to display pagination links even if you do not set the pagination values.
     * @return Links
     */
    public function withPagination()
    {
        $this->hasPagination = true;

        return clone $this;
    }


    /**
     * @param string $url
     * @return Links
     */
    public function withFirst($url)
    {
        $this->hasPagination = true;
        if ($this->validateUrl($url)) {
            $this->first = $url;
        } else {
            $this->getLogger()->error(LinksMessage::INVALID_URL_PARAMETER);
        }

        return clone $this;
    }

    /**
     * @param string $url
     * @return Links
     */
    public function withPrev($url)
    {
        $this->hasPagination = true;
        if ($this->validateUrl($url)) {
            $this->prev = $url;
        } else {
            $this->getLogger()->error(LinksMessage::INVALID_URL_PARAMETER);
        }

        return clone $this;
    }

    /**
     * @param string $url
     * @return Links
     */
    public function withNext($url)
    {
        $this->hasPagination = true;
        if ($this->validateUrl($url)) {
            $this->next = $url;
        } else {
            $this->getLogger()->error(LinksMessage::INVALID_URL_PARAMETER);
        }

        return clone $this;
    }

    /**
     * @param string $url
     * @return Links
     */
    public function withLast($url)
    {
        $this->hasPagination = true;
        if ($this->validateUrl($url)) {
            $this->last = $url;
        } else {
            $this->getLogger()->error(LinksMessage::INVALID_URL_PARAMETER);
        }

        return clone $this;
    }

    /**
     * @param array|null $meta
     * @return Links
     */
    public function withMeta($meta)
    {
        if ($this->meta === null) {
            $this->meta = $meta;
        } else {
            $this->meta = array_merge($this->meta, $meta);
        }

        return clone $this;
    }


    /**
     * @param string $url
     * @return Links
     */
    public function withHref($url)
    {
        $this->href = $url;

        return clone $this;
    }

    /**
     * @param string $related
     * @return Links
     */
    public function withRelated($related)
    {
        $this->related = $related;

        return clone $this;
    }

    /**
     * @return array
     */
    public function toJsonApiResponse()
    {
        $response = array();
        if ($this->hasSelf()) {
            $response['self'] = $this->self;
        }

        if ($this->hasHref()) {
            $response['href'] = $this->href;
        }

        if ($this->hasMeta()) {
            $response['meta'] = $this->meta;
        }

        if ($this->hasPagination()) {
            if ($this->first !== null) {
                $response['first'] = $this->first;
            }

            if ($this->prev !== null) {
                $response['prev'] = $this->prev;
            }

            if ($this->next !== null) {
                $response['next'] = $this->next;
            }

            if ($this->last !== null) {
                $response['last'] = $this->last;
            }
        }

        if ($this->hasRelated()) {
            $response['related'] = $this->related;
        }

        return $response;
    }

    /**
     * @return bool
     */
    private function hasPagination()
    {
        return $this->hasPagination;
    }

    /**
     * @return bool
     */
    private function hasSelf()
    {
        return $this->self !== null;
    }

    /**
     * @return bool
     */
    private function hasHref()
    {
        return $this->href !== null;
    }

    /**
     * @return bool
     */
    private function hasMeta()
    {
        return $this->meta !== null;
    }

    /**
     * @return bool
     */
    private function hasRelated()
    {
        return $this->related !== null;
    }

    /**
     * @param string $url
     * @return bool true === valid, false === invalid
     */
    private function validateUrl($url)
    {
        $isValid = filter_var(
            $url,
            FILTER_VALIDATE_URL
        );

        return false !== $isValid;
    }

    /**
     * Sets a logger instance on the object.
     *
     * @param LoggerInterface $logger
     *
     * @return void
     */
    public function setLogger(LoggerInterface $logger)
    {
        $this->logger = $logger;
    }
    
    
    /**
     * @return LoggerInterface
     */
    public function getLogger()
    {
        if (!$this->logger) {
            $this->setLogger(new Logger());
        }
        return $this->logger;
    }
}
