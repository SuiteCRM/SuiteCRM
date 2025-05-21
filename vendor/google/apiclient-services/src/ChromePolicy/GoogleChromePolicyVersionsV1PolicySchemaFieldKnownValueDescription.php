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

namespace Google\Service\ChromePolicy;

class GoogleChromePolicyVersionsV1PolicySchemaFieldKnownValueDescription extends \Google\Collection
{
  protected $collection_key = 'fieldDependencies';
  /**
   * @var string
   */
  public $description;
  protected $fieldDependenciesType = GoogleChromePolicyVersionsV1PolicySchemaFieldDependencies::class;
  protected $fieldDependenciesDataType = 'array';
  /**
   * @var string
   */
  public $value;

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
   * @param GoogleChromePolicyVersionsV1PolicySchemaFieldDependencies[]
   */
  public function setFieldDependencies($fieldDependencies)
  {
    $this->fieldDependencies = $fieldDependencies;
  }
  /**
   * @return GoogleChromePolicyVersionsV1PolicySchemaFieldDependencies[]
   */
  public function getFieldDependencies()
  {
    return $this->fieldDependencies;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromePolicyVersionsV1PolicySchemaFieldKnownValueDescription::class, 'Google_Service_ChromePolicy_GoogleChromePolicyVersionsV1PolicySchemaFieldKnownValueDescription');
