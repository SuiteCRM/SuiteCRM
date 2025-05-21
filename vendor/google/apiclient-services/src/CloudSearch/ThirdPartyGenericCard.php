<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\CloudSearch;

class ThirdPartyGenericCard extends \Google\Model
{
  /**
   * @var string
   */
  public $cardId;
  /**
   * @var string
   */
  public $category;
  protected $contentType = Content::class;
  protected $contentDataType = '';
  protected $contextType = Context::class;
  protected $contextDataType = '';
  /**
   * @var bool
   */
  public $isDismissible;
  /**
   * @var int
   */
  public $priority;

  /**
   * @param string
   */
  public function setCardId($cardId)
  {
    $this->cardId = $cardId;
  }
  /**
   * @return string
   */
  public function getCardId()
  {
    return $this->cardId;
  }
  /**
   * @param string
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return string
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param Content
   */
  public function setContent(Content $content)
  {
    $this->content = $content;
  }
  /**
   * @return Content
   */
  public function getContent()
  {
    return $this->content;
  }
  /**
   * @param Context
   */
  public function setContext(Context $context)
  {
    $this->context = $context;
  }
  /**
   * @return Context
   */
  public function getContext()
  {
    return $this->context;
  }
  /**
   * @param bool
   */
  public function setIsDismissible($isDismissible)
  {
    $this->isDismissible = $isDismissible;
  }
  /**
   * @return bool
   */
  public function getIsDismissible()
  {
    return $this->isDismissible;
  }
  /**
   * @param int
   */
  public function setPriority($priority)
  {
    $this->priority = $priority;
  }
  /**
   * @return int
   */
  public function getPriority()
  {
    return $this->priority;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ThirdPartyGenericCard::class, 'Google_Service_CloudSearch_ThirdPartyGenericCard');
