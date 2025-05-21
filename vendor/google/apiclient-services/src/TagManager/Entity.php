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

namespace Google\Service\TagManager;

class Entity extends \Google\Model
{
  protected $builtInVariableType = BuiltInVariable::class;
  protected $builtInVariableDataType = '';
  /**
   * @var string
   */
  public $changeStatus;
  protected $clientType = Client::class;
  protected $clientDataType = '';
  protected $customTemplateType = CustomTemplate::class;
  protected $customTemplateDataType = '';
  protected $folderType = Folder::class;
  protected $folderDataType = '';
  protected $gtagConfigType = GtagConfig::class;
  protected $gtagConfigDataType = '';
  protected $tagType = Tag::class;
  protected $tagDataType = '';
  protected $transformationType = Transformation::class;
  protected $transformationDataType = '';
  protected $triggerType = Trigger::class;
  protected $triggerDataType = '';
  protected $variableType = Variable::class;
  protected $variableDataType = '';
  protected $zoneType = Zone::class;
  protected $zoneDataType = '';

  /**
   * @param BuiltInVariable
   */
  public function setBuiltInVariable(BuiltInVariable $builtInVariable)
  {
    $this->builtInVariable = $builtInVariable;
  }
  /**
   * @return BuiltInVariable
   */
  public function getBuiltInVariable()
  {
    return $this->builtInVariable;
  }
  /**
   * @param string
   */
  public function setChangeStatus($changeStatus)
  {
    $this->changeStatus = $changeStatus;
  }
  /**
   * @return string
   */
  public function getChangeStatus()
  {
    return $this->changeStatus;
  }
  /**
   * @param Client
   */
  public function setClient(Client $client)
  {
    $this->client = $client;
  }
  /**
   * @return Client
   */
  public function getClient()
  {
    return $this->client;
  }
  /**
   * @param CustomTemplate
   */
  public function setCustomTemplate(CustomTemplate $customTemplate)
  {
    $this->customTemplate = $customTemplate;
  }
  /**
   * @return CustomTemplate
   */
  public function getCustomTemplate()
  {
    return $this->customTemplate;
  }
  /**
   * @param Folder
   */
  public function setFolder(Folder $folder)
  {
    $this->folder = $folder;
  }
  /**
   * @return Folder
   */
  public function getFolder()
  {
    return $this->folder;
  }
  /**
   * @param GtagConfig
   */
  public function setGtagConfig(GtagConfig $gtagConfig)
  {
    $this->gtagConfig = $gtagConfig;
  }
  /**
   * @return GtagConfig
   */
  public function getGtagConfig()
  {
    return $this->gtagConfig;
  }
  /**
   * @param Tag
   */
  public function setTag(Tag $tag)
  {
    $this->tag = $tag;
  }
  /**
   * @return Tag
   */
  public function getTag()
  {
    return $this->tag;
  }
  /**
   * @param Transformation
   */
  public function setTransformation(Transformation $transformation)
  {
    $this->transformation = $transformation;
  }
  /**
   * @return Transformation
   */
  public function getTransformation()
  {
    return $this->transformation;
  }
  /**
   * @param Trigger
   */
  public function setTrigger(Trigger $trigger)
  {
    $this->trigger = $trigger;
  }
  /**
   * @return Trigger
   */
  public function getTrigger()
  {
    return $this->trigger;
  }
  /**
   * @param Variable
   */
  public function setVariable(Variable $variable)
  {
    $this->variable = $variable;
  }
  /**
   * @return Variable
   */
  public function getVariable()
  {
    return $this->variable;
  }
  /**
   * @param Zone
   */
  public function setZone(Zone $zone)
  {
    $this->zone = $zone;
  }
  /**
   * @return Zone
   */
  public function getZone()
  {
    return $this->zone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Entity::class, 'Google_Service_TagManager_Entity');
