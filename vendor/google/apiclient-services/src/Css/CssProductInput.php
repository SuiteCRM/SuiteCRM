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

namespace Google\Service\Css;

class CssProductInput extends \Google\Collection
{
  protected $collection_key = 'customAttributes';
  protected $attributesType = Attributes::class;
  protected $attributesDataType = '';
  /**
   * @var string
   */
  public $contentLanguage;
  protected $customAttributesType = CustomAttribute::class;
  protected $customAttributesDataType = 'array';
  /**
   * @var string
   */
  public $feedLabel;
  /**
   * @var string
   */
  public $finalName;
  /**
   * @var string
   */
  public $freshnessTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $rawProvidedId;

  /**
   * @param Attributes
   */
  public function setAttributes(Attributes $attributes)
  {
    $this->attributes = $attributes;
  }
  /**
   * @return Attributes
   */
  public function getAttributes()
  {
    return $this->attributes;
  }
  /**
   * @param string
   */
  public function setContentLanguage($contentLanguage)
  {
    $this->contentLanguage = $contentLanguage;
  }
  /**
   * @return string
   */
  public function getContentLanguage()
  {
    return $this->contentLanguage;
  }
  /**
   * @param CustomAttribute[]
   */
  public function setCustomAttributes($customAttributes)
  {
    $this->customAttributes = $customAttributes;
  }
  /**
   * @return CustomAttribute[]
   */
  public function getCustomAttributes()
  {
    return $this->customAttributes;
  }
  /**
   * @param string
   */
  public function setFeedLabel($feedLabel)
  {
    $this->feedLabel = $feedLabel;
  }
  /**
   * @return string
   */
  public function getFeedLabel()
  {
    return $this->feedLabel;
  }
  /**
   * @param string
   */
  public function setFinalName($finalName)
  {
    $this->finalName = $finalName;
  }
  /**
   * @return string
   */
  public function getFinalName()
  {
    return $this->finalName;
  }
  /**
   * @param string
   */
  public function setFreshnessTime($freshnessTime)
  {
    $this->freshnessTime = $freshnessTime;
  }
  /**
   * @return string
   */
  public function getFreshnessTime()
  {
    return $this->freshnessTime;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setRawProvidedId($rawProvidedId)
  {
    $this->rawProvidedId = $rawProvidedId;
  }
  /**
   * @return string
   */
  public function getRawProvidedId()
  {
    return $this->rawProvidedId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CssProductInput::class, 'Google_Service_Css_CssProductInput');
