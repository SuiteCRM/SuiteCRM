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

namespace Google\Service\NetworkServices;

class WasmPluginVersion extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $imageDigest;
  /**
   * @var string
   */
  public $imageUri;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $pluginConfigData;
  /**
   * @var string
   */
  public $pluginConfigDigest;
  /**
   * @var string
   */
  public $pluginConfigUri;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setImageDigest($imageDigest)
  {
    $this->imageDigest = $imageDigest;
  }
  /**
   * @return string
   */
  public function getImageDigest()
  {
    return $this->imageDigest;
  }
  /**
   * @param string
   */
  public function setImageUri($imageUri)
  {
    $this->imageUri = $imageUri;
  }
  /**
   * @return string
   */
  public function getImageUri()
  {
    return $this->imageUri;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setPluginConfigData($pluginConfigData)
  {
    $this->pluginConfigData = $pluginConfigData;
  }
  /**
   * @return string
   */
  public function getPluginConfigData()
  {
    return $this->pluginConfigData;
  }
  /**
   * @param string
   */
  public function setPluginConfigDigest($pluginConfigDigest)
  {
    $this->pluginConfigDigest = $pluginConfigDigest;
  }
  /**
   * @return string
   */
  public function getPluginConfigDigest()
  {
    return $this->pluginConfigDigest;
  }
  /**
   * @param string
   */
  public function setPluginConfigUri($pluginConfigUri)
  {
    $this->pluginConfigUri = $pluginConfigUri;
  }
  /**
   * @return string
   */
  public function getPluginConfigUri()
  {
    return $this->pluginConfigUri;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WasmPluginVersion::class, 'Google_Service_NetworkServices_WasmPluginVersion');
