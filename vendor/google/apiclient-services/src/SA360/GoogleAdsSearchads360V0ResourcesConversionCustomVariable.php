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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesConversionCustomVariable extends \Google\Collection
{
  protected $collection_key = 'customColumnIds';
  /**
   * @var string
   */
  public $cardinality;
  /**
   * @var string[]
   */
  public $customColumnIds;
  /**
   * @var string
   */
  public $family;
  protected $floodlightConversionCustomVariableInfoType = GoogleAdsSearchads360V0ResourcesConversionCustomVariableFloodlightConversionCustomVariableInfo::class;
  protected $floodlightConversionCustomVariableInfoDataType = '';
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $ownerCustomer;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $tag;

  /**
   * @param string
   */
  public function setCardinality($cardinality)
  {
    $this->cardinality = $cardinality;
  }
  /**
   * @return string
   */
  public function getCardinality()
  {
    return $this->cardinality;
  }
  /**
   * @param string[]
   */
  public function setCustomColumnIds($customColumnIds)
  {
    $this->customColumnIds = $customColumnIds;
  }
  /**
   * @return string[]
   */
  public function getCustomColumnIds()
  {
    return $this->customColumnIds;
  }
  /**
   * @param string
   */
  public function setFamily($family)
  {
    $this->family = $family;
  }
  /**
   * @return string
   */
  public function getFamily()
  {
    return $this->family;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversionCustomVariableFloodlightConversionCustomVariableInfo
   */
  public function setFloodlightConversionCustomVariableInfo(GoogleAdsSearchads360V0ResourcesConversionCustomVariableFloodlightConversionCustomVariableInfo $floodlightConversionCustomVariableInfo)
  {
    $this->floodlightConversionCustomVariableInfo = $floodlightConversionCustomVariableInfo;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversionCustomVariableFloodlightConversionCustomVariableInfo
   */
  public function getFloodlightConversionCustomVariableInfo()
  {
    return $this->floodlightConversionCustomVariableInfo;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
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
  public function setOwnerCustomer($ownerCustomer)
  {
    $this->ownerCustomer = $ownerCustomer;
  }
  /**
   * @return string
   */
  public function getOwnerCustomer()
  {
    return $this->ownerCustomer;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setTag($tag)
  {
    $this->tag = $tag;
  }
  /**
   * @return string
   */
  public function getTag()
  {
    return $this->tag;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesConversionCustomVariable::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesConversionCustomVariable');
