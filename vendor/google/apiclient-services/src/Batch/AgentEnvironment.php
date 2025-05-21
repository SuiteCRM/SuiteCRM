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

namespace Google\Service\Batch;

class AgentEnvironment extends \Google\Model
{
  protected $encryptedVariablesType = AgentKMSEnvMap::class;
  protected $encryptedVariablesDataType = '';
  /**
   * @var string[]
   */
  public $secretVariables;
  /**
   * @var string[]
   */
  public $variables;

  /**
   * @param AgentKMSEnvMap
   */
  public function setEncryptedVariables(AgentKMSEnvMap $encryptedVariables)
  {
    $this->encryptedVariables = $encryptedVariables;
  }
  /**
   * @return AgentKMSEnvMap
   */
  public function getEncryptedVariables()
  {
    return $this->encryptedVariables;
  }
  /**
   * @param string[]
   */
  public function setSecretVariables($secretVariables)
  {
    $this->secretVariables = $secretVariables;
  }
  /**
   * @return string[]
   */
  public function getSecretVariables()
  {
    return $this->secretVariables;
  }
  /**
   * @param string[]
   */
  public function setVariables($variables)
  {
    $this->variables = $variables;
  }
  /**
   * @return string[]
   */
  public function getVariables()
  {
    return $this->variables;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AgentEnvironment::class, 'Google_Service_Batch_AgentEnvironment');
