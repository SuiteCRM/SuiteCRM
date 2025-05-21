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

class WasmPlugin extends \Google\Collection
{
  protected $collection_key = 'usedBy';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string[]
   */
  public $labels;
  protected $logConfigType = WasmPluginLogConfig::class;
  protected $logConfigDataType = '';
  /**
   * @var string
   */
  public $mainVersionId;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $updateTime;
  protected $usedByType = WasmPluginUsedBy::class;
  protected $usedByDataType = 'array';
  protected $versionsType = WasmPluginVersionDetails::class;
  protected $versionsDataType = 'map';

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
   * @param WasmPluginLogConfig
   */
  public function setLogConfig(WasmPluginLogConfig $logConfig)
  {
    $this->logConfig = $logConfig;
  }
  /**
   * @return WasmPluginLogConfig
   */
  public function getLogConfig()
  {
    return $this->logConfig;
  }
  /**
   * @param string
   */
  public function setMainVersionId($mainVersionId)
  {
    $this->mainVersionId = $mainVersionId;
  }
  /**
   * @return string
   */
  public function getMainVersionId()
  {
    return $this->mainVersionId;
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
  /**
   * @param WasmPluginUsedBy[]
   */
  public function setUsedBy($usedBy)
  {
    $this->usedBy = $usedBy;
  }
  /**
   * @return WasmPluginUsedBy[]
   */
  public function getUsedBy()
  {
    return $this->usedBy;
  }
  /**
   * @param WasmPluginVersionDetails[]
   */
  public function setVersions($versions)
  {
    $this->versions = $versions;
  }
  /**
   * @return WasmPluginVersionDetails[]
   */
  public function getVersions()
  {
    return $this->versions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WasmPlugin::class, 'Google_Service_NetworkServices_WasmPlugin');
