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

class GoogleCloudAiplatformV1ExamplesRestrictionsNamespace extends \Google\Collection
{
  protected $collection_key = 'deny';
  /**
   * @var string[]
   */
  public $allow;
  /**
   * @var string[]
   */
  public $deny;
  /**
   * @var string
   */
  public $namespaceName;

  /**
   * @param string[]
   */
  public function setAllow($allow)
  {
    $this->allow = $allow;
  }
  /**
   * @return string[]
   */
  public function getAllow()
  {
    return $this->allow;
  }
  /**
   * @param string[]
   */
  public function setDeny($deny)
  {
    $this->deny = $deny;
  }
  /**
   * @return string[]
   */
  public function getDeny()
  {
    return $this->deny;
  }
  /**
   * @param string
   */
  public function setNamespaceName($namespaceName)
  {
    $this->namespaceName = $namespaceName;
  }
  /**
   * @return string
   */
  public function getNamespaceName()
  {
    return $this->namespaceName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExamplesRestrictionsNamespace::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExamplesRestrictionsNamespace');
