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

namespace Google\Service\CloudBuild;

class Step extends \Google\Collection
{
  protected $collection_key = 'volumeMounts';
  /**
   * @var string[]
   */
  public $args;
  /**
   * @var string[]
   */
  public $command;
  protected $envType = EnvVar::class;
  protected $envDataType = 'array';
  /**
   * @var string
   */
  public $image;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $onError;
  protected $paramsType = Param::class;
  protected $paramsDataType = 'array';
  protected $refType = StepRef::class;
  protected $refDataType = '';
  /**
   * @var string
   */
  public $script;
  protected $securityContextType = SecurityContext::class;
  protected $securityContextDataType = '';
  /**
   * @var string
   */
  public $timeout;
  protected $volumeMountsType = VolumeMount::class;
  protected $volumeMountsDataType = 'array';
  /**
   * @var string
   */
  public $workingDir;

  /**
   * @param string[]
   */
  public function setArgs($args)
  {
    $this->args = $args;
  }
  /**
   * @return string[]
   */
  public function getArgs()
  {
    return $this->args;
  }
  /**
   * @param string[]
   */
  public function setCommand($command)
  {
    $this->command = $command;
  }
  /**
   * @return string[]
   */
  public function getCommand()
  {
    return $this->command;
  }
  /**
   * @param EnvVar[]
   */
  public function setEnv($env)
  {
    $this->env = $env;
  }
  /**
   * @return EnvVar[]
   */
  public function getEnv()
  {
    return $this->env;
  }
  /**
   * @param string
   */
  public function setImage($image)
  {
    $this->image = $image;
  }
  /**
   * @return string
   */
  public function getImage()
  {
    return $this->image;
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
  public function setOnError($onError)
  {
    $this->onError = $onError;
  }
  /**
   * @return string
   */
  public function getOnError()
  {
    return $this->onError;
  }
  /**
   * @param Param[]
   */
  public function setParams($params)
  {
    $this->params = $params;
  }
  /**
   * @return Param[]
   */
  public function getParams()
  {
    return $this->params;
  }
  /**
   * @param StepRef
   */
  public function setRef(StepRef $ref)
  {
    $this->ref = $ref;
  }
  /**
   * @return StepRef
   */
  public function getRef()
  {
    return $this->ref;
  }
  /**
   * @param string
   */
  public function setScript($script)
  {
    $this->script = $script;
  }
  /**
   * @return string
   */
  public function getScript()
  {
    return $this->script;
  }
  /**
   * @param SecurityContext
   */
  public function setSecurityContext(SecurityContext $securityContext)
  {
    $this->securityContext = $securityContext;
  }
  /**
   * @return SecurityContext
   */
  public function getSecurityContext()
  {
    return $this->securityContext;
  }
  /**
   * @param string
   */
  public function setTimeout($timeout)
  {
    $this->timeout = $timeout;
  }
  /**
   * @return string
   */
  public function getTimeout()
  {
    return $this->timeout;
  }
  /**
   * @param VolumeMount[]
   */
  public function setVolumeMounts($volumeMounts)
  {
    $this->volumeMounts = $volumeMounts;
  }
  /**
   * @return VolumeMount[]
   */
  public function getVolumeMounts()
  {
    return $this->volumeMounts;
  }
  /**
   * @param string
   */
  public function setWorkingDir($workingDir)
  {
    $this->workingDir = $workingDir;
  }
  /**
   * @return string
   */
  public function getWorkingDir()
  {
    return $this->workingDir;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Step::class, 'Google_Service_CloudBuild_Step');
