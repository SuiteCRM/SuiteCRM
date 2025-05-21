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

namespace Google\Service\Pollen;

class IndexInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $category;
  /**
   * @var string
   */
  public $code;
  protected $colorType = Color::class;
  protected $colorDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $indexDescription;
  /**
   * @var int
   */
  public $value;

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
   * @param string
   */
  public function setCode($code)
  {
    $this->code = $code;
  }
  /**
   * @return string
   */
  public function getCode()
  {
    return $this->code;
  }
  /**
   * @param Color
   */
  public function setColor(Color $color)
  {
    $this->color = $color;
  }
  /**
   * @return Color
   */
  public function getColor()
  {
    return $this->color;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setIndexDescription($indexDescription)
  {
    $this->indexDescription = $indexDescription;
  }
  /**
   * @return string
   */
  public function getIndexDescription()
  {
    return $this->indexDescription;
  }
  /**
   * @param int
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return int
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IndexInfo::class, 'Google_Service_Pollen_IndexInfo');
