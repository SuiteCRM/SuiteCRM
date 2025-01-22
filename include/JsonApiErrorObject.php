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

namespace SuiteCRM;

use Exception;
use Psr\Http\Message\ServerRequestInterface;
use SuiteCRM\API\v8\Exception\ApiException;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}



/**
 * implementation of http://jsonapi.org/format/#error-objects
 *
 * @todo incomplete...
 */
#[\AllowDynamicProperties]
class JsonApiErrorObject
{

    /**
     * integer
     */
    public const DEFAULT_ID = 1;

    /**
     * integer
     */
    public const DEFAULT_CODE = 1;

    /**
     * integer
     */
    public const DEFAULT_STATUS = 200;

    /**
     *
     * @var string
     */
    protected $id;

    /**
     *
     * @var array
     */
    protected $links;

    /**
     *
     * @var string
     */
    protected $status;

    /**
     *
     * @var string
     */
    protected $code;

    /**
     *
     * @var LangText
     */
    protected $title;

    /**
     *
     * @var LangText
     */
    protected $detail;

    /**
     *
     * @var array
     */
    protected $source;

    /**
     *
     * @var array
     */
    protected $meta;

    /**
     *
     * @param \SuiteCRM\LangText|null $title
     * @param \SuiteCRM\LangText|null $detail
     * @param string $id
     * @param string $code
     * @param string $status
     * @param array $links
     * @param array $source
     * @param array $meta
     */
    public function __construct(LangText $title = null, LangText $detail = null, $id = null, $code = null, $status = null, $links = null, $source = null, $meta = null)
    {
        $this->setTitle($title ? $title : $this->getDefaultTitle());
        $this->setDetail($detail ? $detail : $this->getDefaultDetail());
        $this->setId($id ? $id : $this->getDefaultId());
        $this->setCode($code ? $code : $this->getDefaultCode());
        $this->setStatus($status ? $status : $this->getDefaultStatus());
        $this->setLinks($links ? $links : $this->getDefaultLinks());
        $this->setSource($source ? $source : $this->getDefaultSource());
        $this->setMeta($meta ? $meta : $this->getDefaultMeta());
    }

    /**
     *
     * @param LangText $title
     */
    public function setTitle(LangText $title)
    {
        if ($this->isValidTitle($title)) {
            $this->title = $title->getText();
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid title', 'warn');
            $this->title = $this->getDefaultTitle();
        }
    }

    /**
     *
     * @param LangText $detail
     */
    public function setDetail(LangText $detail)
    {
        if ($this->isValidDetail($detail)) {
            $this->detail = $detail->getText();
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid detail', 'warn');
            $this->detail = $this->getDefaultDetail();
        }
    }

    /**
     *
     * @param string $id
     */
    public function setId($id)
    {
        if ($this->isValidId($id)) {
            $this->id = $id;
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid id', 'warn');
            $this->getDefaultId();
        }
    }

    /**
     *
     * @param string $code
     */
    public function setCode($code)
    {
        if ($this->isValidCode($code)) {
            $this->code = $code;
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid code', 'warn');
            $this->code = $this->getDefaultCode();
        }
    }

    /**
     *
     * @param string $status
     */
    public function setStatus($status)
    {
        if ($this->isValidStatus($status)) {
            $this->status = $status;
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid status', 'warn');
            $this->status = $this->getDefaultSource();
        }
    }

    /**
     *
     * @param array $links
     */
    public function setLinks($links)
    {
        if ($this->isValidLinks($links)) {
            $this->links = $links;
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid links', 'warn');
            $this->links = $this->getDefaultLinks();
        }
    }

    /**
     *
     * @param array $source
     */
    public function setSource($source)
    {
        if ($this->isValidSource($source)) {
            $this->source = $source;
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid source', 'warn');
            $this->source = $this->getDefaultSource();
        }
    }

    /**
     *
     * @param array $meta
     */
    public function setMeta($meta)
    {
        if ($this->getValidMeta($meta)) {
            $this->meta = $meta;
        } else {
            ErrorMessage::log('Invalid JSON API error object, invalid meta', 'warn');
            $this->meta = $this->getDefaultMeta();
        }
    }

    /**
     *
     * @param LangText $title
     * @return boolean
     */
    protected function isValidTitle(LangText $title)
    {
        return true;
    }

    /**
     *
     * @param LangText $detail
     * @return boolean
     */
    protected function isValidDetail(LangText $detail)
    {
        return true;
    }

    /**
     *
     * @param string $id
     * @return boolean
     */
    protected function isValidId($id)
    {
        return is_string($id) || is_numeric($id);
    }

    /**
     *
     * @param string $code
     * @return boolean
     */
    protected function isValidCode($code)
    {
        return is_string($code) || is_numeric($code);
    }

    /**
     *
     * @param string $status
     * @return boolean
     */
    protected function isValidStatus($status)
    {
        return is_string($status) || is_numeric($status);
    }

    /**
     *
     * @param array $links
     * @return boolean
     */
    protected function isValidLinks($links)
    {
        return is_array($links) && isset($links['about']);
    }

    /**
     *
     * @param array $source
     * @return boolean
     */
    protected function isValidSource($source)
    {
        return is_array($source) && isset($source['pointer']) && isset($source['parameter']);
    }

    /**
     *
     * @param array $meta
     * @return boolean
     */
    protected function getValidMeta($meta)
    {
        return is_array($meta);
    }

    /**
     *
     * @return LangText
     */
    protected function getDefaultTitle()
    {
        $text = new LangText('LBL_DEFAULT_API_ERROR_TITLE');
        return $text;
    }

    /**
     *
     * @return LangText
     */
    protected function getDefaultDetail()
    {
        $text = new LangText('LBL_DEFAULT_API_ERROR_DETAIL');
        return $text;
    }

    /**
     *
     * @return string
     */
    protected function getDefaultId()
    {
        return (string) self::DEFAULT_ID;
    }

    /**
     *
     * @return string
     */
    protected function getDefaultCode()
    {
        return (string) self::DEFAULT_CODE;
    }

    /**
     *
     * @return string
     */
    protected function getDefaultStatus()
    {
        return (string) self::DEFAULT_STATUS;
    }

    /**
     *
     * @return array
     */
    protected function getDefaultLinks()
    {
        return ['about' => null];
    }

    /**
     *
     * @return array
     */
    protected function getDefaultSource()
    {
        return ['pointer' => null, 'parameter' => null];
    }

    /**
     *
     * @return array
     */
    protected function getDefaultMeta()
    {
        return [];
    }
    
    /**
     *
     * @return string|null
     */
    protected function getId()
    {
        return $this->id;
    }
    
    /**
     *
     * @return array|null
     */
    protected function getLinks()
    {
        return $this->links;
    }
    
    /**
     *
     * @return string|null
     */
    protected function getStatus()
    {
        return $this->status;
    }
    
    /**
     *
     * @return string|null
     */
    protected function getCode()
    {
        return $this->code;
    }
    
    /**
     *
     * @return string|null
     */
    protected function getTitle()
    {
        return $this->title;
    }
    
    /**
     *
     * @return string|null
     */
    protected function getDetail()
    {
        return $this->detail;
    }
    
    /**
     *
     * @return array|null
     */
    protected function getSource()
    {
        return $this->source;
    }
    
    /**
     *
     * @return array|null
     */
    protected function getMeta()
    {
        return $this->meta;
    }

    /**
     *
     * @return array
     */
    public function export()
    {
        return [
            'id' => $this->getId(),
            'links' => $this->getLinks(),
            'status' => $this->getStatus(),
            'code' => $this->getCode(),
            'title' => $this->getTitle(),
            'detail' => $this->getDetail(),
            'source' => $this->getSource(),
            'meta' => $this->getMeta(),
        ];
    }

    /**
     *
     * @return string
     */
    public function exportJson()
    {
        $json = json_encode($this->export());
        $error = json_last_error();
        if ($error !== JSON_ERROR_NONE) {
            ErrorMessage::log('API Error Object JSON export error: ' . json_last_error_msg());
        }
        return $json;
    }
    
    /**
     *
     * @global array $sugar_config
     * @param Exception $e
     * @return array
     */
    protected function retrieveMetaFromException(Exception $e)
    {
        $meta = [
            'about' => 'Exception',
            'class' => get_class($e),
            'code' => $e->getCode(),
        ];
        
        if ($e instanceof LangExceptionInterface) {
            $meta['langMessage'] = $e->getLangMessage();
        }

        if (inDeveloperMode()) {
            $meta['debug']['message'] = $e->getMessage();
            $meta['debug']['file'] = $e->getFile();
            $meta['debug']['line'] = $e->getLine();
            $meta['debug']['trace'] = $e->getTrace();
            $meta['debug']['traceAsString'] = $e->getTraceAsString();
            if ($previous = $e->getPrevious()) {
                $meta['debug']['previous'] = $this->retrieveMetaFromException($previous);
            }
        }
        
        return $meta;
    }
    
    /**
     *
     * @param Exception $e
     * @return $this
     */
    public function retrieveFromException(Exception $e)
    {
        $this->setCode($e->getCode());
        
        $meta = $this->retrieveMetaFromException($e);
        
        $this->setMeta($meta);
        
        
        if ($e instanceof ApiException) {
            $this->setCode($e->getCode());
            $this->setStatus($e->getHttpStatus());
            $this->setDetail($e->getDetail());
            $this->setStatus($e->getHttpStatus());
        }
        
        return $this;
    }
    
    /**
     *
     * @param ServerRequestInterface $request
     * @return $this
     */
    public function retrieveFromRequest(ServerRequestInterface $request)
    {
        $this->setSource([
            'pointer' => $request->getUri(),
            'parametes' => $request->getQueryParams(),
        ]);
        
        return $this;
    }
}
