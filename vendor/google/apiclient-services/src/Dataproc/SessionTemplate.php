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

namespace Google\Service\Dataproc;

class SessionTemplate extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $creator;
  /**
   * @var string
   */
  public $description;
  protected $environmentConfigType = EnvironmentConfig::class;
  protected $environmentConfigDataType = '';
  protected $jupyterSessionType = JupyterConfig::class;
  protected $jupyterSessionDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $runtimeConfigType = RuntimeConfig::class;
  protected $runtimeConfigDataType = '';
  protected $sparkConnectSessionType = SparkConnectConfig::class;
  protected $sparkConnectSessionDataType = '';
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $uuid;

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
  public function setCreator($creator)
  {
    $this->creator = $creator;
  }
  /**
   * @return string
   */
  public function getCreator()
  {
    return $this->creator;
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
   * @param EnvironmentConfig
   */
  public function setEnvironmentConfig(EnvironmentConfig $environmentConfig)
  {
    $this->environmentConfig = $environmentConfig;
  }
  /**
   * @return EnvironmentConfig
   */
  public function getEnvironmentConfig()
  {
    return $this->environmentConfig;
  }
  /**
   * @param JupyterConfig
   */
  public function setJupyterSession(JupyterConfig $jupyterSession)
  {
    $this->jupyterSession = $jupyterSession;
  }
  /**
   * @return JupyterConfig
   */
  public function getJupyterSession()
  {
    return $this->jupyterSession;
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
   * @param RuntimeConfig
   */
  public function setRuntimeConfig(RuntimeConfig $runtimeConfig)
  {
    $this->runtimeConfig = $runtimeConfig;
  }
  /**
   * @return RuntimeConfig
   */
  public function getRuntimeConfig()
  {
    return $this->runtimeConfig;
  }
  /**
   * @param SparkConnectConfig
   */
  public function setSparkConnectSession(SparkConnectConfig $sparkConnectSession)
  {
    $this->sparkConnectSession = $sparkConnectSession;
  }
  /**
   * @return SparkConnectConfig
   */
  public function getSparkConnectSession()
  {
    return $this->sparkConnectSession;
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
   * @param string
   */
  public function setUuid($uuid)
  {
    $this->uuid = $uuid;
  }
  /**
   * @return string
   */
  public function getUuid()
  {
    return $this->uuid;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SessionTemplate::class, 'Google_Service_Dataproc_SessionTemplate');
