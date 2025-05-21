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

class GoogleCloudAiplatformV1ReasoningEngineSpec extends \Google\Collection
{
  protected $collection_key = 'classMethods';
  /**
   * @var array[]
   */
  public $classMethods;
  protected $packageSpecType = GoogleCloudAiplatformV1ReasoningEngineSpecPackageSpec::class;
  protected $packageSpecDataType = '';

  /**
   * @param array[]
   */
  public function setClassMethods($classMethods)
  {
    $this->classMethods = $classMethods;
  }
  /**
   * @return array[]
   */
  public function getClassMethods()
  {
    return $this->classMethods;
  }
  /**
   * @param GoogleCloudAiplatformV1ReasoningEngineSpecPackageSpec
   */
  public function setPackageSpec(GoogleCloudAiplatformV1ReasoningEngineSpecPackageSpec $packageSpec)
  {
    $this->packageSpec = $packageSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1ReasoningEngineSpecPackageSpec
   */
  public function getPackageSpec()
  {
    return $this->packageSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ReasoningEngineSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ReasoningEngineSpec');
