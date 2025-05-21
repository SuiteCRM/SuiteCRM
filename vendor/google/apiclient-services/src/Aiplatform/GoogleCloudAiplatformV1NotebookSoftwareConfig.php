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

class GoogleCloudAiplatformV1NotebookSoftwareConfig extends \Google\Collection
{
  protected $collection_key = 'env';
  protected $envType = GoogleCloudAiplatformV1EnvVar::class;
  protected $envDataType = 'array';
  protected $postStartupScriptConfigType = GoogleCloudAiplatformV1PostStartupScriptConfig::class;
  protected $postStartupScriptConfigDataType = '';

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
   * @param GoogleCloudAiplatformV1PostStartupScriptConfig
   */
  public function setPostStartupScriptConfig(GoogleCloudAiplatformV1PostStartupScriptConfig $postStartupScriptConfig)
  {
    $this->postStartupScriptConfig = $postStartupScriptConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1PostStartupScriptConfig
   */
  public function getPostStartupScriptConfig()
  {
    return $this->postStartupScriptConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NotebookSoftwareConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NotebookSoftwareConfig');
