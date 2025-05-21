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

class GoogleCloudAiplatformV1PostStartupScriptConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $postStartupScript;
  /**
   * @var string
   */
  public $postStartupScriptBehavior;
  /**
   * @var string
   */
  public $postStartupScriptUrl;

  /**
   * @param string
   */
  public function setPostStartupScript($postStartupScript)
  {
    $this->postStartupScript = $postStartupScript;
  }
  /**
   * @return string
   */
  public function getPostStartupScript()
  {
    return $this->postStartupScript;
  }
  /**
   * @param string
   */
  public function setPostStartupScriptBehavior($postStartupScriptBehavior)
  {
    $this->postStartupScriptBehavior = $postStartupScriptBehavior;
  }
  /**
   * @return string
   */
  public function getPostStartupScriptBehavior()
  {
    return $this->postStartupScriptBehavior;
  }
  /**
   * @param string
   */
  public function setPostStartupScriptUrl($postStartupScriptUrl)
  {
    $this->postStartupScriptUrl = $postStartupScriptUrl;
  }
  /**
   * @return string
   */
  public function getPostStartupScriptUrl()
  {
    return $this->postStartupScriptUrl;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PostStartupScriptConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PostStartupScriptConfig');
