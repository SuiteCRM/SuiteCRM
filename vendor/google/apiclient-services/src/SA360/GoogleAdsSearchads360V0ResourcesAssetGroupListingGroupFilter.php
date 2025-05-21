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

class GoogleAdsSearchads360V0ResourcesAssetGroupListingGroupFilter extends \Google\Model
{
  /**
   * @var string
   */
  public $assetGroup;
  protected $caseValueType = GoogleAdsSearchads360V0ResourcesListingGroupFilterDimension::class;
  protected $caseValueDataType = '';
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $parentListingGroupFilter;
  protected $pathType = GoogleAdsSearchads360V0ResourcesListingGroupFilterDimensionPath::class;
  protected $pathDataType = '';
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $vertical;

  /**
   * @param string
   */
  public function setAssetGroup($assetGroup)
  {
    $this->assetGroup = $assetGroup;
  }
  /**
   * @return string
   */
  public function getAssetGroup()
  {
    return $this->assetGroup;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesListingGroupFilterDimension
   */
  public function setCaseValue(GoogleAdsSearchads360V0ResourcesListingGroupFilterDimension $caseValue)
  {
    $this->caseValue = $caseValue;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesListingGroupFilterDimension
   */
  public function getCaseValue()
  {
    return $this->caseValue;
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
  public function setParentListingGroupFilter($parentListingGroupFilter)
  {
    $this->parentListingGroupFilter = $parentListingGroupFilter;
  }
  /**
   * @return string
   */
  public function getParentListingGroupFilter()
  {
    return $this->parentListingGroupFilter;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesListingGroupFilterDimensionPath
   */
  public function setPath(GoogleAdsSearchads360V0ResourcesListingGroupFilterDimensionPath $path)
  {
    $this->path = $path;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesListingGroupFilterDimensionPath
   */
  public function getPath()
  {
    return $this->path;
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
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setVertical($vertical)
  {
    $this->vertical = $vertical;
  }
  /**
   * @return string
   */
  public function getVertical()
  {
    return $this->vertical;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesAssetGroupListingGroupFilter::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesAssetGroupListingGroupFilter');
