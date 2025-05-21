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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1PythonPackageSpec extends \Google\Collection
{
  protected $collection_key = 'packageUris';
  /**
   * @var string[]
   */
  public $args;
  protected $envType = GoogleCloudAiplatformV1EnvVar::class;
  protected $envDataType = 'array';
  /**
   * @var string
   */
  public $executorImageUri;
  /**
   * @var string[]
   */
  public $packageUris;
  /**
   * @var string
   */
  public $pythonModule;

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
   * @param GoogleCloudAiplatformV1EnvVar[]
   */
  public function setEnv($env)
  {
    $this->env = $env;
  }
  /**
   * @return GoogleCloudAiplatformV1EnvVar[]
   */
  public function getEnv()
  {
    return $this->env;
  }
  /**
   * @param string
   */
  public function setExecutorImageUri($executorImageUri)
  {
    $this->executorImageUri = $executorImageUri;
  }
  /**
   * @return string
   */
  public function getExecutorImageUri()
  {
    return $this->executorImageUri;
  }
  /**
   * @param string[]
   */
  public function setPackageUris($packageUris)
  {
    $this->packageUris = $packageUris;
  }
  /**
   * @return string[]
   */
  public function getPackageUris()
  {
    return $this->packageUris;
  }
  /**
   * @param string
   */
  public function setPythonModule($pythonModule)
  {
    $this->pythonModule = $pythonModule;
  }
  /**
   * @return string
   */
  public function getPythonModule()
  {
    return $this->pythonModule;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PythonPackageSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PythonPackageSpec');
