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

namespace Google\Service\Config;

class ConfigResource extends \Google\Model
{
  protected $caiAssetsType = ResourceCAIInfo::class;
  protected $caiAssetsDataType = 'map';
  /**
   * @var string
   */
  public $intent;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;
  protected $terraformInfoType = ResourceTerraformInfo::class;
  protected $terraformInfoDataType = '';

  /**
   * @param ResourceCAIInfo[]
   */
  public function setCaiAssets($caiAssets)
  {
    $this->caiAssets = $caiAssets;
  }
  /**
   * @return ResourceCAIInfo[]
   */
  public function getCaiAssets()
  {
    return $this->caiAssets;
  }
  /**
   * @param string
   */
  public function setIntent($intent)
  {
    $this->intent = $intent;
  }
  /**
   * @return string
   */
  public function getIntent()
  {
    return $this->intent;
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
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param ResourceTerraformInfo
   */
  public function setTerraformInfo(ResourceTerraformInfo $terraformInfo)
  {
    $this->terraformInfo = $terraformInfo;
  }
  /**
   * @return ResourceTerraformInfo
   */
  public function getTerraformInfo()
  {
    return $this->terraformInfo;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConfigResource::class, 'Google_Service_Config_ConfigResource');
