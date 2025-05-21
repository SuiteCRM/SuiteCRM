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

namespace Google\Service\MigrationCenterAPI;

class RemoveAssetsFromGroupRequest extends \Google\Model
{
  /**
   * @var bool
   */
  public $allowMissing;
  protected $assetsType = AssetList::class;
  protected $assetsDataType = '';
  /**
   * @var string
   */
  public $requestId;

  /**
   * @param bool
   */
  public function setAllowMissing($allowMissing)
  {
    $this->allowMissing = $allowMissing;
  }
  /**
   * @return bool
   */
  public function getAllowMissing()
  {
    return $this->allowMissing;
  }
  /**
   * @param AssetList
   */
  public function setAssets(AssetList $assets)
  {
    $this->assets = $assets;
  }
  /**
   * @return AssetList
   */
  public function getAssets()
  {
    return $this->assets;
  }
  /**
   * @param string
   */
  public function setRequestId($requestId)
  {
    $this->requestId = $requestId;
  }
  /**
   * @return string
   */
  public function getRequestId()
  {
    return $this->requestId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RemoveAssetsFromGroupRequest::class, 'Google_Service_MigrationCenterAPI_RemoveAssetsFromGroupRequest');
