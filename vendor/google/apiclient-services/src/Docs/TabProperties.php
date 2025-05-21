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

namespace Google\Service\Docs;

class TabProperties extends \Google\Model
{
  /**
   * @var int
   */
  public $index;
  /**
   * @var int
   */
  public $nestingLevel;
  /**
   * @var string
   */
  public $parentTabId;
  /**
   * @var string
   */
  public $tabId;
  /**
   * @var string
   */
  public $title;

  /**
   * @param int
   */
  public function setIndex($index)
  {
    $this->index = $index;
  }
  /**
   * @return int
   */
  public function getIndex()
  {
    return $this->index;
  }
  /**
   * @param int
   */
  public function setNestingLevel($nestingLevel)
  {
    $this->nestingLevel = $nestingLevel;
  }
  /**
   * @return int
   */
  public function getNestingLevel()
  {
    return $this->nestingLevel;
  }
  /**
   * @param string
   */
  public function setParentTabId($parentTabId)
  {
    $this->parentTabId = $parentTabId;
  }
  /**
   * @return string
   */
  public function getParentTabId()
  {
    return $this->parentTabId;
  }
  /**
   * @param string
   */
  public function setTabId($tabId)
  {
    $this->tabId = $tabId;
  }
  /**
   * @return string
   */
  public function getTabId()
  {
    return $this->tabId;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TabProperties::class, 'Google_Service_Docs_TabProperties');
