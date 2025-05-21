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

namespace Google\Service\Dataflow;

class TemplateMetadata extends \Google\Collection
{
  protected $collection_key = 'parameters';
  /**
   * @var string
   */
  public $defaultStreamingMode;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $name;
  protected $parametersType = ParameterMetadata::class;
  protected $parametersDataType = 'array';
  /**
   * @var bool
   */
  public $streaming;
  /**
   * @var bool
   */
  public $supportsAtLeastOnce;
  /**
   * @var bool
   */
  public $supportsExactlyOnce;
  /**
   * @var string
   */
  public $yamlDefinition;

  /**
   * @param string
   */
  public function setDefaultStreamingMode($defaultStreamingMode)
  {
    $this->defaultStreamingMode = $defaultStreamingMode;
  }
  /**
   * @return string
   */
  public function getDefaultStreamingMode()
  {
    return $this->defaultStreamingMode;
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
   * @param ParameterMetadata[]
   */
  public function setParameters($parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return ParameterMetadata[]
   */
  public function getParameters()
  {
    return $this->parameters;
  }
  /**
   * @param bool
   */
  public function setStreaming($streaming)
  {
    $this->streaming = $streaming;
  }
  /**
   * @return bool
   */
  public function getStreaming()
  {
    return $this->streaming;
  }
  /**
   * @param bool
   */
  public function setSupportsAtLeastOnce($supportsAtLeastOnce)
  {
    $this->supportsAtLeastOnce = $supportsAtLeastOnce;
  }
  /**
   * @return bool
   */
  public function getSupportsAtLeastOnce()
  {
    return $this->supportsAtLeastOnce;
  }
  /**
   * @param bool
   */
  public function setSupportsExactlyOnce($supportsExactlyOnce)
  {
    $this->supportsExactlyOnce = $supportsExactlyOnce;
  }
  /**
   * @return bool
   */
  public function getSupportsExactlyOnce()
  {
    return $this->supportsExactlyOnce;
  }
  /**
   * @param string
   */
  public function setYamlDefinition($yamlDefinition)
  {
    $this->yamlDefinition = $yamlDefinition;
  }
  /**
   * @return string
   */
  public function getYamlDefinition()
  {
    return $this->yamlDefinition;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TemplateMetadata::class, 'Google_Service_Dataflow_TemplateMetadata');
