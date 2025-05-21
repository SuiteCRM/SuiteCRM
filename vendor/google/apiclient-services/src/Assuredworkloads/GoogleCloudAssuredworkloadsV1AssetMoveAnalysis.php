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

namespace Google\Service\Assuredworkloads;

class GoogleCloudAssuredworkloadsV1AssetMoveAnalysis extends \Google\Collection
{
  protected $collection_key = 'analysisGroups';
  protected $analysisGroupsType = GoogleCloudAssuredworkloadsV1MoveAnalysisGroup::class;
  protected $analysisGroupsDataType = 'array';
  /**
   * @var string
   */
  public $asset;
  /**
   * @var string
   */
  public $assetType;

  /**
   * @param GoogleCloudAssuredworkloadsV1MoveAnalysisGroup[]
   */
  public function setAnalysisGroups($analysisGroups)
  {
    $this->analysisGroups = $analysisGroups;
  }
  /**
   * @return GoogleCloudAssuredworkloadsV1MoveAnalysisGroup[]
   */
  public function getAnalysisGroups()
  {
    return $this->analysisGroups;
  }
  /**
   * @param string
   */
  public function setAsset($asset)
  {
    $this->asset = $asset;
  }
  /**
   * @return string
   */
  public function getAsset()
  {
    return $this->asset;
  }
  /**
   * @param string
   */
  public function setAssetType($assetType)
  {
    $this->assetType = $assetType;
  }
  /**
   * @return string
   */
  public function getAssetType()
  {
    return $this->assetType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAssuredworkloadsV1AssetMoveAnalysis::class, 'Google_Service_Assuredworkloads_GoogleCloudAssuredworkloadsV1AssetMoveAnalysis');
